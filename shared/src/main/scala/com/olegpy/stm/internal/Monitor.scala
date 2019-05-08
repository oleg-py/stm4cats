package com.olegpy.stm.internal

import cats.effect.implicits._
import cats.effect.Concurrent
import cats.implicits._



class Monitor private[stm] () {
  type Callback = Either[Throwable, Unit] => Unit
  private[this] val store: Store = /*_*/Store.forPlatform()/*_*/
  private[this] val rightUnit = Right(())

  private[this] class RetryCallback(keys: Iterable[AnyRef]) {
    @volatile var catsCb: Callback = _
    keys.foreach(listenTo)
    def invoke(): Unit = {
      catsCb(rightUnit)
    }

    def listenTo(key: AnyRef): Unit = {
      addToSet(key, this)
      addToSet(this, key)
    }

    private[this] def addToSet(key: AnyRef, value: Any): Unit = {
      val j = store.current()
      j.update(key, j.read(key) match {
        case set: Set[Any @unchecked] => set + value
        case _ => Set(value)
      })
    }

    private[this] def removeFromSet(key: AnyRef, value: Any): Unit = {
      val j = store.current()
      j.update(key, j.read(key) match {
        case set: Set[Any @unchecked] => set - value
        case other => other
      })
    }

    def removeAllKeys(): Unit = {
      val j = store.current()
      j.read(this) match {
        case set: Set[AnyRef @unchecked] => set.foreach(removeFromSet(_, this))
        // This might not be hit in a single test run, avoid fluctuating coverage
        // $COVERAGE-OFF$
        case _ =>
        // $COVERAGE-ON$
      }
      j.update(this, null)
    }
  }

  store.transact(store.current().update(this, 0))

  def lastNotify: Int = store.transact { lastNotifyT }
  private def lastNotifyT = store.current().read(this).asInstanceOf[Int]

  def waitOn[F[_]](ln: Int, keys: Iterable[AnyRef])(implicit F: Concurrent[F]): F[Unit] =
    F.suspend { store.transact {
      if (ln == lastNotifyT) {
        val rc = new RetryCallback(keys)
        F.cancelable[Unit] { cb =>
          rc.catsCb = cb
          F.delay { store.transact(rc.removeAllKeys()) }
        }
      } else F.unit
    }}

  def notifyOn[F[_]](keys: Iterable[AnyRef])(implicit F: Concurrent[F]): F[Unit] = F.suspend {
    store.transact {
      val j = store.current()
      j.update(this, lastNotifyT + 1)
      val cbs = Set.newBuilder[RetryCallback]
      keys.foreach { key =>
        j.read(key) match {
          case s: Set[RetryCallback @unchecked] => cbs ++= s
          case _ =>
        }
      }
      val allJobs = cbs.result()
      if (allJobs.isEmpty) F.unit
      else {
        allJobs.foreach(_.removeAllKeys())
        F.delay(allJobs.foreach(_.invoke())).start.void
      }
    }
  }
}
