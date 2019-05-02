package com.olegpy.stm

import cats.effect.IO
import utest._
import cats.implicits._

object RollbackTests extends TestSuite with BaseIOSuite {
  val tests = Tests {
    "commit fails on exceptions" - {
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

    "commit fails on aborts" - {
      for {
        s <- TRef.in[IO](0)
        _ <- STM.atomically[IO] {
          s.set(5) >> STM.abort(new Exception("Transaction aborted"))
        }.attempt
        r <- s.get.commit[IO]
      } yield assert(r == 0)
    }
  }
}
