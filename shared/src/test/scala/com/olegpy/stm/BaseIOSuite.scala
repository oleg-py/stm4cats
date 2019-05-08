package com.olegpy.stm

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

import cats.effect.laws.util.TestContext
import cats.implicits._
import cats.effect.{ContextShift, IO, Timer}
import utest._

trait IOSuiteUtils {
  def timer: Timer[IO]
  def nap: IO[Unit] = timer.sleep(10.millis)

  def longNap: IO[Unit] = nap.replicateA(10).void

  def fail[A]: IO[A] = IO.suspend {
    assert(false)
    IO.never // unreachable, but above has type Unit
  }

  def disabled(a: => Any): Unit = ()

  val number = 42
}

abstract class NondetIOSuite extends TestSuite with IOSuiteUtils {
  def ec: ExecutionContext = ExecutionContext.global
  implicit def cs: ContextShift[IO] = IO.contextShift(ec)
  implicit def timer: Timer[IO] = IO.timer(ec)


  def ioTimeout: FiniteDuration = 750.millis

  override def utestWrap(path: Seq[String], runBody: => Future[Any])(implicit ec: ExecutionContext): Future[Any] = {
    super.utestWrap(path, runBody.flatMap {
      case io: IO[_] => io.timeout(ioTimeout).unsafeToFuture()
      case other => Future.successful(other)
    })(ec)
  }
}

abstract class DeterministicIOSuite extends TestSuite with IOSuiteUtils {
  private[this] val tc = TestContext()
  implicit def cs: ContextShift[IO] = tc.contextShift[IO](IO.ioEffect)
  implicit def timer: Timer[IO] = tc.timer[IO](IO.ioEffect)

  override def utestWrap(path: Seq[String], runBody: => Future[Any])(implicit ec: ExecutionContext): Future[Any] = {
    super.utestWrap(path, runBody.flatMap {
      case io: IO[_] =>
        val f = io.unsafeToFuture()
        tc.tick(365.days)
        Future.fromTry(f.value.get)
      case other => Future.successful(other)
    })(new ExecutionContext {
      def execute(runnable: Runnable): Unit = runnable.run()
      def reportFailure(cause: Throwable): Unit = throw cause
    })
  }
}