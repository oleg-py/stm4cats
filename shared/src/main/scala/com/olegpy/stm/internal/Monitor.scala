package com.olegpy.stm.internal

import scala.collection.immutable.Queue

import cats.effect.implicits._
import cats.effect.Concurrent
import cats.implicits._



class Monitor private[stm] () {
  type Callback = Either[Throwable, Unit] => Unit
  private[this] val store: Store = /*_*/Store.forPlatform()/*_*/
  private[this] val rightUnit = Right(())

  private[this] class RetryCallback (catsCb: Callback) {
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
        case set: Queue[Any @unchecked] => set.enqueue(value)
        case _ => Queue(value)
      })
    }

    private[this] def removeFromSet(key: AnyRef, value: Any): Unit = {
      val j = store.current()
      j.update(key, j.read(key) match {
        case set: Queue[Any @unchecked] => set.filter(_ != value)
        case other => other
      })
    }

    def removeAllKeys(): Unit = {
      val j = store.current()
      j.read(this) match {
        case set: Queue[AnyRef @unchecked] => set.foreach(removeFromSet(_, this))
        case _ =>
      }
      j.update(this, null)
    }
  }

  def waitOn[F[_]](keys: Iterable[AnyRef])(implicit F: Concurrent[F]): F[Unit] = F.cancelable[Unit] { cb =>
    val retryCallback = store.transact {
      val rc = new RetryCallback(cb)
      keys.foreach(rc.listenTo)
      rc
    }
    F.delay { store.transact(retryCallback.removeAllKeys()) }
  }

  def notifyOn[F[_]](keys: Iterable[AnyRef])(implicit F: Concurrent[F]): F[Unit] = F.suspend {
    store.transact {
      val j = store.current()
      val cbs = List.newBuilder[RetryCallback]
      keys.foreach { key =>
        j.read(key) match {
          case q: Queue[RetryCallback @unchecked] => cbs ++= q
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
