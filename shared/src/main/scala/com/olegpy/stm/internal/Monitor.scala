package com.olegpy.stm.internal

import scala.collection.immutable.Queue

import cats.effect.implicits._
import cats.effect.Concurrent
import cats.implicits._


private[stm] class Monitor {
  type Callback = Either[Throwable, Unit] => Unit
  private[this] val store: Store = /*_*/Store.forPlatform()/*_*/
  private[this] val rightUnit = Right(())

  case class PendingUpdate(lastNotifyVersion: Long, cbs: Queue[Callback] = Queue.empty)
  private[this] val emptyPU = PendingUpdate(Long.MinValue)
  private[this] def read(k: AnyRef): PendingUpdate = store.current().read(k) match {
    case p: PendingUpdate => p
    case _ => emptyPU
  }

  private[this] def register(k: AnyRef, cb: Callback): Unit = {
    val PendingUpdate(lnv, cbs) = read(k)
    store.current().update(k, PendingUpdate(lnv, cbs enqueue cb))
  }

  private[this] def unsub(cb: Callback): Unit = {
    val j = store.current()
    val keys = j.read(cb).asInstanceOf[collection.Set[AnyRef @unchecked]]
    j.update(cb, null) // TODO - wipe?
    val kit = // $COVERAGE-OFF$
      if (keys eq null) Iterator.empty else keys.iterator
      // $COVERAGE-ON$
    while (kit.hasNext) {
      val k = kit.next()
      val PendingUpdate(lnv, cbs) = j.read(k)
      j.update(k, PendingUpdate(lnv, cbs.diff(List(cb))))
    }
  }

  def waitOn[F[_]](lastSeen: collection.Map[AnyRef, Long])(implicit F: Concurrent[F]): F[Unit] =
    F.cancelable { cb =>
      store.transact {
        var abort = false
        val it = lastSeen.iterator
        while (it.hasNext && !abort) {
          val (k, ver) = it.next()
          if (ver < read(k).lastNotifyVersion) abort = true
        }
        if (abort) () => {
          cb(rightUnit); F.unit
        }
        else {
          store.current().update(cb, lastSeen.keySet)
          val it = lastSeen.iterator
          while (it.hasNext) register(it.next()._1, cb)
          () => F.delay {
            store.transact { unsub(cb) }
          }
        }
      }.apply()
    }

  def notifyOn[F[_]](versions: collection.Map[AnyRef, Long])(implicit F: Concurrent[F]): F[Unit] =
    F.suspend {
      store.transact {
        val qb = Queue.newBuilder[Callback]
        val it = versions.iterator
        val j = store.current()
        while (it.hasNext) {
          val (k, ver) = it.next()
          val PendingUpdate(v2, cbs) = read(k)
          if (ver > v2) {
            qb ++= cbs
            j.update(k, PendingUpdate(ver))
          }
        }
        val callbacks = qb.result()
        val qit = callbacks.iterator
        while (qit.hasNext) {
          unsub(qit.next())
        }

        if (callbacks.isEmpty) F.unit
        else F.delay(callbacks.foreach(_(rightUnit))).start.void
      }
    }
}
