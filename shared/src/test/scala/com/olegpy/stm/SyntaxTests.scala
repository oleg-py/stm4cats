package com.olegpy.stm

import cats.effect.{IO, SyncIO}
import utest._
import cats.implicits._

object SyntaxTests extends TestSuite with BaseIOSuite {
  val tests = Tests {
    "STM.atomically" - STM.atomically[IO](STM.unit)
    "TRef.apply" - TRef(5)
    "TRef.in" - TRef.in[SyncIO](15)
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
