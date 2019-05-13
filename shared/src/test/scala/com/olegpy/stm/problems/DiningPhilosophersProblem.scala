package com.olegpy.stm.problems

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import com.olegpy.stm.misc.TMVar
import com.olegpy.stm.{NondetIOSuite, STM}
import utest._

object DiningPhilosophersProblem extends NondetIOSuite {
  val tests = Tests {
    "Dining philosophers problem" - {
      val philCount = 8
      val iterCount = 40

      val mkCycle = for {
        philosophers <- Ref[IO].of(0).map(new Philosopher(_)).replicateA(philCount)
        leftForks    <- TMVar.in[IO, Unit](()).replicateA(philCount)
        rightForks    = leftForks.tail :+ leftForks.head
      } yield (philosophers, leftForks, rightForks)
        .parMapN(_.eat(_, _)) // Parallel for List is ZipList, so we zip all forks together
        .parSequence_ // This is IO's Parallel now, do everything concurrently
        .>>(philosophers.traverse(_.timesEaten)) // get values out for assertions

      mkCycle.flatMap { cycle =>
        def loop(n: Int): IO[Unit] =
          if (n == iterCount) IO.unit
          else cycle.map { list =>
            assert(list.length == philCount)
            assert(list.forall(_ == n))
          } >> loop(n + 1)
        loop(1)
      }
    }

    type Fork = TMVar[Unit]

    class Philosopher (timesEatenRef: Ref[IO, Int]) {
      val timesEaten = timesEatenRef.get
      def eat(left: Fork, right: Fork): IO[Unit] = for {
        // It is ABSOLUTELY essential to solution to do takes atomically. We either take
        // both, or take nothing
        _ <- STM.atomically[IO] { left.take >> right.take }
        // Pretend we're eating
        _ <- IO.shift
        _ <- timesEatenRef.update(_ + 1)
        // Putting them back atomically isn't strictly necessary, it's just a micro-
        // optimization to reduce # of transactions and also just looks more brief
        _ <- STM.atomically[IO] { left.put(()) >> right.put(()) }
      } yield ()
    }
  }
}
