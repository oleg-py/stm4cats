package com.olegpy.stm

import scala.concurrent.ExecutionContext

import cats.effect.{ContextShift, IO}
import utest._
import cats.implicits._

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger


object ConcurrentTests extends TestSuite with BaseIOSuite {
  val tests = Tests {
    "concurrent transactions can complete w/o reevaluation" - ioTest {
      val mkEc = IO {
        ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
      }
      val attempts = new AtomicInteger()
      def test(ref: TRef[Int], ec: ExecutionContext)(implicit
        cs: ContextShift[IO]) = cs.evalOn(ec) {
          ref.modify(_ + 1).map { _ =>
            Thread.sleep(100)
            attempts.incrementAndGet()
          }.commit[IO]
        }.start

      val ecs = 3

      for {
        refs   <- TRef.in[IO](0).product(mkEc).replicateA(ecs)
        fibers <- refs.traverse(Function.tupled(test(_, _)))
        _      <- fibers.sequence.join
      } yield assert(attempts.get() == ecs)
    }
  }
}
