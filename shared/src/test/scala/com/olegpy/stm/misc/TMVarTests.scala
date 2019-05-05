package com.olegpy.stm.misc

import cats.effect.IO
import com.olegpy.stm.BaseIOSuite
import com.olegpy.stm.results._
import utest._

object TMVarTests extends TestSuite with BaseIOSuite {
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
  }
}
