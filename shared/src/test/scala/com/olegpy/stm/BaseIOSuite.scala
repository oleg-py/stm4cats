package com.olegpy.stm

import scala.concurrent.{ExecutionContext, Future}

import cats.effect.{ContextShift, IO, Timer}
import utest._

trait BaseIOSuite {
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  def ioTest[A](io: IO[A]): Future[A] = io.unsafeToFuture()

  def fail: IO[Nothing] = IO.suspend {
    assert(false)
    IO.never // unreachable, but above has type Unit
  }
}
