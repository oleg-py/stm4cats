package com.olegpy.stm

import cats.effect.{IO, SyncIO}
import utest._
import cats.implicits._

object SyntaxTests extends TestSuite with BaseIOSuite {
  val tests = Tests {
    "STM.atomically" - {
      STM.atomically[IO](STM.pure(number))
        .map { _ ==> number }
    }

    "TRef.apply" - {
      TRef(number).flatMap(_.get).commit[IO]
        .map { _ ==> number }
    }

    "TRef.in" - {
      for {
        tr <- TRef.in[SyncIO](number).toIO
        x  <- tr.get.commit[IO]
      } yield assert(x == number)
    }

    "for-comprehension with guards" - {
      def transfer(from: TRef[Int], to: TRef[Int], amt: Int): STM[Unit] =
        for {
          balance <- from.get
          if balance >= amt
          _  <- from.modify(_ - amt)
          _  <- to.modify(_ + amt)
        } yield ()

      IO(transfer(null, null, 0))
      ()
    }
  }
}
