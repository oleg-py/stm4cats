package com.olegpy.stm

import scala.concurrent.duration._

import cats.effect.IO
import utest._
import cats.implicits._

object RetryTests extends TestSuite with BaseIOSuite {
  def tests: Tests = Tests {
    "Retry doesn't terminate" - ioTest {
      IO.race(STM.retry.commit[IO], IO.sleep(100.millis)) map { _ ==> Right(()) }
    }

    "Retrying eventually completes, if possible" - ioTest {
      for {
        ref <- TRef.in[IO](0)
        inc <- (ref.modify(_ + 1).commit[IO] *> IO.sleep(10.millis)).replicateA(5).start
        x   <- ref.get.filter(_ >= 5).commit[IO]
        _   <- inc.cancel
      } yield assert(x == 5)
    }

    "orElse falls back to first successful" - ioTest {
      for {
        x <- STM.retry.orElse(STM.pure(42)).commit[IO]
      } yield assert(x == 42)
    }
  }
}
