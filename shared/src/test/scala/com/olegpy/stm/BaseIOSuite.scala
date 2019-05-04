package com.olegpy.stm

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

import cats.implicits._
import cats.effect.{ContextShift, IO, Timer}
import utest._

trait BaseIOSuite extends TestSuite {
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  val number = 42

  def ioTimeout: FiniteDuration = 750.millis

  override def utestWrap(path: Seq[String], runBody: => Future[Any])(implicit ec: ExecutionContext): Future[Any] = {
    super.utestWrap(path, runBody.flatMap {
      case io: IO[_] => io.timeout(ioTimeout).unsafeToFuture()
      case other => Future.successful(other)
    })(ec)
  }

  def ioTestTimed[A](timeout: FiniteDuration)(io: IO[A]): Future[A] =
    io.timeout(timeout).unsafeToFuture()

  def nap: IO[Unit] = IO.sleep(10.millis)

  def longNap: IO[Unit] = nap.replicateA(10).void

  def fail[A]: IO[A] = IO.suspend {
    assert(false)
    IO.never // unreachable, but above has type Unit
  }
}
