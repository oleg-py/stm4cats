package com.olegpy.stm.internal

import cats.effect.implicits._
import cats.effect.{Concurrent, Sync}
import cats.implicits._

import java.util.concurrent.atomic.AtomicReference


class Monitor private[stm] () {
  private[this] val pendings = new AtomicReference(List.empty[() => Unit])

  def waitF[F[_]](implicit F: Concurrent[F]): F[Unit] = F.cancelable[Unit] { cb =>
      val trigger = () => cb(Right(()))
      pendings.updateAndGet(trigger :: _)
      F.delay { pendings.updateAndGet(_.filterNot(_ eq trigger)); () }
  }

  def notifyOneF[F[_]](implicit F: Concurrent[F]): F[Unit] = F.suspend {
    pendings.getAndUpdate(_ drop 1) match {
      case trigger :: _ => F.delay(trigger()).start.void
      case _            => F.unit
    }
  }
}

object Monitor {
  def apply[F[_]: Sync]: F[Monitor] = Sync[F].delay(new Monitor)
}