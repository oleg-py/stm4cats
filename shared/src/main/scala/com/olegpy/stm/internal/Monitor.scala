package com.olegpy.stm.internal

import cats.effect.implicits._
import cats.effect.Concurrent
import cats.implicits._



class Monitor private[stm] () {
  type Callback = Either[Throwable, Unit] => Unit
  private[this] val store: Store = /*_*/Store.forPlatform()/*_*/
  private[this] val rightUnit = Right(())

  private[this] class RetryCallback (catsCb: Callback, keys: Iterable[AnyRef]) {
    keys.foreach(addToSet(_, this))
    def invoke(): Unit = {
      catsCb(rightUnit)
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
      keys.foreach(removeFromSet(_, this))
    }
  }

  def waitOn[F[_]](keys: Iterable[AnyRef])(implicit F: Concurrent[F]): F[Unit] = Concurrent.cancelableF[F, Unit] { cb =>
    store.transact {
      val ks = keys.toSet
      store.current().read(this) match {
        case l: List[Set[AnyRef] @unchecked] if l.exists(_.exists(ks)) =>
          null
        case _ =>
          val retryCallback = new RetryCallback(cb, keys)
          F.delay { store.transact(retryCallback.removeAllKeys()) }
      }
    } match {
      case null =>
        F.delay { cb(rightUnit) }.start.map(_.cancel)
      case token: F[Unit] @unchecked => F.pure(token)
    }
  }

  def notifyOn[F[_]](keys: Iterable[AnyRef])(implicit F: Concurrent[F]): F[Unit] = F.suspend {
    val ks = keys.toSet

    def unregister(): Unit = {
      store.transact {
        val j = store.current()
        j.read(this) match {
          case l: List[Set[AnyRef] @unchecked] => j.update(this, l.filterNot(_ == ks))
          case _ =>
        }
      }
    }

    val jobs = store.transact {
      val j = store.current()
      j.update(this, j.read(this) match {
        case l: List[Set[AnyRef] @unchecked] => ks :: l
        case _ => ks :: Nil
      })
      val cbs = Set.newBuilder[RetryCallback]
      keys.foreach { key =>
        j.read(key) match {
          case s: Set[RetryCallback @unchecked] => cbs ++= s
          case _ =>
        }
      }

      val allJobs = cbs.result()
      allJobs.foreach(_.removeAllKeys())
      allJobs
    }

    if (jobs.isEmpty) {
      unregister()
      F.unit
    }
    else F.delay {
      unregister()
      jobs.foreach(_.invoke())
    }.start.void
  }
}
