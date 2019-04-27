package com.olegpy.stm

import cats.effect.{ExitCase, IO}
import utest._
import cats.implicits._
import scala.concurrent.duration._

object EdgeCasesTests extends TestSuite with BaseIOSuite {
  val tests = Tests {
    "commit fails on exceptions" - ioTest {
      val Dummy = new Exception()
      def crash(): Unit = throw Dummy
      TRef.in[IO](0)
        .mproduct(_.set(5).map(_ => crash()).commit[IO].attempt)
        .flatMap {
          case (tref, Left(Dummy)) => tref.get.commit[IO]
          case _ => fail
        }
        .map { x => assert(x == 0) }
    }

    "retries are actually cancellable" - ioTest {
      for {
        x <- TRef.in[IO](0)
        upd <- (x.modify(_ + 1).commit[IO] >> IO.sleep(20.millis)).replicateA(10).start
        f1  <- x.get.filter(_ == 5).commit[IO]
          .guaranteeCase {
            case ExitCase.Canceled => upd.cancel >> x.set(-1).commit[IO]
            case _ => fail
          }
          .start
        _ <- IO.sleep(1.millisecond)
        _ <- f1.cancel
        _ <- IO.sleep(100.millis)
        res <- x.get.commit[IO]
      } yield assert(res == -1)
    }
  }
}
