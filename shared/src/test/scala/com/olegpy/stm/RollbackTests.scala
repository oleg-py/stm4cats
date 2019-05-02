package com.olegpy.stm

import cats.effect.IO
import utest._
import cats.implicits._
import com.olegpy.stm.results._

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
        .map { _ ==> 0 }
    }

    "commit fails on aborts" - {
      val ex = new Exception("Transaction aborted")
      for {
        s <- TRef.in[IO](0)
        res <- (s.set(number) >> STM.abort(ex)).result
        r <- s.get.commit[IO]
      } yield {
        r ==> 0
        res ==> STMAbort(ex)
      }
    }

    "orElse doesn't fall back for aborted computations" - {
      val ex = new Exception("Transaction aborted")
      for {
        s <- TRef.in[IO](0)
        res <- (s.set(number) >> (STM.abort(ex) orElse STM.unit)).result
        r <- s.get.commit[IO]
      } yield {
        r ==> 0
        res ==> STMAbort(ex)
      }
    }
  }
}
