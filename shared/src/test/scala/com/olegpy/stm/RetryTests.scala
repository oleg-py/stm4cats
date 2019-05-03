package com.olegpy.stm

import cats.effect.concurrent.Ref
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

    "Unconditional retry from local reads only throws an exception" - {
      (TRef(0).flatMap(_.get).filter(_ => false).commit[IO] >> fail[Unit]).recover {
        case _: PotentialDeadlockException => ()
      }
    }

    "Unconditional retry with nonlocal reads doesn't terminate" - {
      val ref = TRef.in[SyncIO](0).unsafeRunSync()
      val txn = ref.get >> STM.retry
      IO.race(txn.commit[IO], longNap) map { _ ==> Right(()) }
    }

    "Retrying eventually completes, if possible" - {
      for {
        ref <- TRef.in[IO](0)
        inc <- (ref.update(_ + 1).commit[IO] *> nap).replicateA(5).start
        x   <- ref.get.filter(_ >= 5).commit[IO]
        _   <- inc.cancel
      } yield x ==> 5
    }

    "orElse falls back to first successful" - {
      for {
        x <- STM.retry.orElse(STM.pure(number).orElse(STM.pure(0))).commit[IO]
      } yield x ==> number
    }

    "orElse rolls the left back on retry" - {
      for {
        ref <- TRef.in[IO](0)
        _   <- STM.atomically[IO](ref.set(number) >> STM.retry orElse STM.unit)
        x   <- ref.get.commit[IO]
      } yield x ==> 0
    }

    "retries are actually cancellable" - {
      for {
        x <- TRef.in[IO](0)
        upd <- (x.update(_ + 1).commit[IO] >> nap).replicateA(10).start
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

    "retries are done in FIFO order" - {
      object Sem {
        private[this] val state = TRef.in[IO](1).unsafeRunSync()
        def withPermit[A](a: IO[A]): IO[A] =
          state.updOrRetry { case n if n > 0 => n - 1 }.commit[IO]
          .bracket(_ => a)(_ => state.update(_ + 1).commit[IO])
      }

      val workers = List.range(0, 10)
      for {
        results <- Ref[IO].of(List.empty[Int])
        fibers  <- workers.traverse(i => Sem.withPermit(results.update(i :: _) <* nap).start <* nap)
        _       <- fibers.parTraverse(_.join)
        out     <- results.get
      } yield out.reverse ==> workers
    }

    "retries are not triggered by writes to independent variables" - {
      @volatile var count = 0
      val r1, r2, r3 = TRef.in[SyncIO](0).unsafeRunSync()
      val txn: STM[Unit] = for {
        i1 <- r1.get
        i2 <- r2.get
        _  = { count += 1 } // side effects to actually track retries
        if i1 < i2
        _  <- r3.get // after-check gets should not affect anything
      } yield ()

      def later(block: => Unit): IO[Unit] = nap >> nap >> IO(block)

      for {
        f <- txn.commit[IO].start
        _ <- later { count ==> 1 } // Tried once, but failed
        _ <- r1.set(number).commit[IO]
        _ <- later { count ==> 2 } // Tried twice, as we modified r1
        _ <- r3.set(number).commit[IO]
        _ <- later { count ==> 2 } // Didn't try again, as we didn't touch r1 or r2
        _ <- r2.set(number + 1).commit[IO]
        _ <- f.join
        _ <- later { count ==> 3 } // Tried again, and should complete at this point
      } yield ()
    }
  }
}
