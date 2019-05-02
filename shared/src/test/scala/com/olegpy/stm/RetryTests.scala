package com.olegpy.stm

import cats.effect.{ExitCase, IO, SyncIO}
import utest._
import cats.implicits._

object RetryTests extends TestSuite with BaseIOSuite {
  def tests: Tests = Tests {
    "Retry with no reads throws an exception" - {
      (STM.retry.commit[IO] >> fail[Unit]).recover {
        case _: PotentialDeadlockException => ()
      }
    }

    "Unconditional retry after read doesn't terminate" - {
      val txn = TRef(0).flatMap(_.get) >> STM.retry
      IO.race(txn.commit[IO], longNap) map { _ ==> Right(()) }
    }

    "Retrying eventually completes, if possible" - {
      for {
        ref <- TRef.in[IO](0)
        inc <- (ref.modify(_ + 1).commit[IO] *> nap).replicateA(5).start
        x   <- ref.get.filter(_ >= 5).commit[IO]
        _   <- inc.cancel
      } yield x ==> 5
    }

    "orElse falls back to first successful" - {
      for {
        x <- STM.retry.orElse(STM.pure(number)).commit[IO]
      } yield x ==> number
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
      } yield res ==> -1
    }

    "retries are not triggered by writes to independent variables" - {
      @volatile var count = 0
      val r1, r2, r3 = TRef.in[SyncIO](0).unsafeRunSync()
      val txn: STM[Unit] = for {
        i1 <- r1.get
        i2 <- r2.get
        _  = { count += 1 } // side effects to actually track retries
        if i1 < i2
      } yield ()

      def later(block: => Unit): IO[Unit] = nap >> IO(block)

      for {
        f <- txn.commit[IO].start
        _ <- later { count ==> 1 } // Tried once, but failed
        _ <- r1.set(number).commit[IO]
        _ <- later { count ==> 2 } // Tried twice, as we modified r1
        _ <- r3.set(number).commit[IO]
        _ <- later { count ==> 2 } // Didn't try again, as we didn't touch r1 or r2
        _ <- r2.set(number + 1).commit[IO]
        _ <- later { count ==> 3 } // Tried again, and should complete at this point
        _ <- f.join
      } yield ()
    }
  }
}
