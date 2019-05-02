package com.olegpy.stm

import cats.effect.{ExitCase, IO}
import utest._
import cats.implicits._

object RetryTests extends TestSuite with BaseIOSuite {
  def tests: Tests = Tests {
    "Retry alone doesn't terminate" - {
      IO.race(STM.retry.commit[IO], nap) map { _ ==> Right(()) }
    }

    "Retrying eventually completes, if possible" - {
      for {
        ref <- TRef.in[IO](0)
        inc <- (ref.modify(_ + 1).commit[IO] *> nap).replicateA(5).start
        x   <- ref.get.filter(_ >= 5).commit[IO]
        _   <- inc.cancel
      } yield assert(x == 5)
    }

    "orElse falls back to first successful" - {
      for {
        x <- STM.retry.orElse(STM.pure(42)).commit[IO]
      } yield assert(x == 42)
    }

    "retries are actually cancellable" - {
      for {
        x <- TRef.in[IO](0)
        upd <- (x.modify(_ + 1).commit[IO] >> nap).replicateA(10).start
        f1  <- x.get.filter(_ == 5).commit[IO]
          .guaranteeCase {
            case ExitCase.Canceled => upd.cancel >> x.set(-1).commit[IO]
            case _ => fail
          }
          .start
        _ <- nap
        _ <- f1.cancel
        _ <- longNap
        res <- x.get.commit[IO]
      } yield assert(res == -1)
    }
  }
}
