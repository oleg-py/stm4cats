package com.olegpy.stm.misc

import cats.effect.IO
import cats.effect.concurrent.MVar
import cats.implicits._
import com.olegpy.stm.NondetIOSuite
import com.olegpy.stm.results._
import utest._

object TMVarTests extends NondetIOSuite {
  val tests = Tests {
    "TMVar#isEmpty" - {
      for {
        mv1 <- TMVar.emptyIn[IO, Unit]
        mv2 <- TMVar.in[IO, Unit](())
        _   <- mv1.isEmpty.commit[IO].map(_ ==> true)
        _   <- mv2.isEmpty.commit[IO].map(_ ==> false)
      } yield ()
    }

    "TMVar#read" - {
      for {
        mv1 <- TMVar.emptyIn[IO, Unit]
        mv2 <- TMVar.in[IO, Unit](())
        _   <- mv1.read.result.map(_.is[STMRetry.type])
        _   <- mv1.read.result.map(_.is[STMRetry.type])
        _   <- mv2.read.result.map(_ ==> STMSuccess(()))
        _   <- mv2.read.result.map(_ ==> STMSuccess(()))
      } yield ()
    }

    "TMVar#tryTake" - {
      for {
        mv1 <- TMVar.emptyIn[IO, Unit].map(_.to[IO])
        mv2 <- TMVar.in[IO, Unit](()).map(_.to[IO])
        _   <- mv1.tryTake.map(_ ==> None)
        _   <- mv1.tryTake.map(_ ==> None)
        _   <- mv2.tryTake.map(_ ==> Some(()))
        _   <- mv2.tryTake.map(_ ==> None)
      } yield ()
    }

    "TMVar#tryPut" - {
      for {
        mv1 <- TMVar.emptyIn[IO, Unit].map(_.to[IO])
        mv2 <- TMVar.in[IO, Unit](()).map(_.to[IO])
        _   <- mv1.tryPut(()).map(_ ==> true)
        _   <- mv1.tryPut(()).map(_ ==> false)
        _   <- mv2.tryPut(()).map(_ ==> false)
        _   <- mv2.tryPut(()).map(_ ==> false)
      } yield ()
    }

    "TMVar#take" - {
      def producer(mv: MVar[IO, Int]): IO[Unit] = mv.put(1) >> mv.put(2) >> mv.put(3)
      def consumer(mv: MVar[IO, Int]): IO[List[Int]] = {
        def loop(list: List[Int]): IO[List[Int]] = {
          if (list.length == 3) IO.pure(list.reverse)
          else nap >> mv.take.map(_ :: list) >>= loop
        }
        loop(Nil)
      }

      for {
        mv   <- TMVar.emptyIn[IO, Int].map(_.to[IO])
        _    <- producer(mv).start
        list <- consumer(mv)
      } yield list ==> List(1, 2, 3)
    }

    "TMVar#toString shows a state" - {
      for {
        mv <- TMVar.emptyIn[IO, Int]
        _  = mv.toString ==> "TMVar(<empty>)"
        _  <- mv.put(number).commit[IO]
        _  = mv.toString ==> s"TMVar(<full: $number>)"
      } yield ()
    }
  }
}
