package com.olegpy.stm

import cats.effect.IO
import utest._
import cats.syntax.apply._

object APITests extends TestSuite with BaseIOSuite {
  val tests = Tests {
    "STM.atomically" - {
      STM.atomically[IO](STM.pure(number))
        .map { _ ==> number }
    }

    "for-comprehension with guards" - {
      def transfer(from: TRef[Int], to: TRef[Int], amt: Int): STM[Unit] =
        for {
          balance <- from.get
          if balance >= amt
          _  <- from.update(_ - amt)
          _  <- to.update(_ + amt)
        } yield ()

      val acc = TRef(number)
      (acc, acc).mapN(transfer(_, _, 10)).commit[IO]
    }

    "SemigroupK syntax" - {
      val ref = TRef.in[IO](10).unsafeRunSync()
      (ref.get <+> STM.retry).commit[IO]
    }

    "STM#unNone" - {
      for {
        ref <- TRef.in[IO](Option(number))
        x   <- ref.get.unNone.commit[IO]
      } yield assert(x == number)
    }

    "STM#filterNot" - {
      STM.pure(number).filterNot(_ != number).commit[IO].map { _ ==> number }
    }

    "STM.atomicallyK" - {
      val fk = STM.atomicallyK[IO]
      fk(STM.pure(number)).map(_ ==> number)
    }

    "STM#iterateUntilRetry" - {
      for {
        r <- TRef.in[IO](4)
        x <- r.modify(x => (x - 1, x)).filter(_ > 0).iterateUntilRetry.commit[IO]
      } yield x ==> List(4, 3, 2, 1)
    }
  }
}
