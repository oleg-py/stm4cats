package com.olegpy.stm.internal

import scala.annotation.tailrec

import cats.effect.implicits._
import cats.effect.Concurrent
import cats.implicits._

import java.util.concurrent.atomic.AtomicReference


class Monitor private[stm] () {
  type Todos = List[() => Unit]
  private[this] val pendings = new AtomicReference[Todos](List.empty)

  @tailrec private[this] def getAndUpdate(f: Todos => Todos): Todos = {
    val current = pendings.get()
    val result = f(current)
    if (pendings.compareAndSet(current, result)) current
    else getAndUpdate(f)
  }

  def waitF[F[_]](implicit F: Concurrent[F]): F[Unit] = F.cancelable[Unit] { cb =>
      val trigger = () => cb(Right(()))
      getAndUpdate(trigger :: _)
      F.delay { getAndUpdate(_.filterNot(_ eq trigger)); () }
  }

  def notifyOneF[F[_]](implicit F: Concurrent[F]): F[Unit] = F.suspend {
    getAndUpdate(_ drop 1) match {
      case trigger :: _ => F.delay(trigger()).start.void
      case _            => F.unit
    }
  }
}
