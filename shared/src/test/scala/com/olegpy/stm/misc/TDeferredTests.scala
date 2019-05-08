package com.olegpy.stm.misc

import cats.effect.IO
import cats.implicits._
import com.olegpy.stm.NondetIOSuite
import com.olegpy.stm.results._
import utest._


object TDeferredTests extends NondetIOSuite {
  val ofUnit = TDeferred.in[IO, Unit].map(_.in[IO])
  val tests = Tests {
    "TDeferred.tryGet works as with regular TryableDeferred" - {
      for {
        d1 <- ofUnit
        r1 <- d1.tryGet
        _  = r1 ==> None
        _  <- d1.complete(())
        r2 <- d1.tryGet
        _ = r2 ==> Some(())
      } yield ()
    }

    "TDeferred.get semantically blocks" - {
      for {
        d1 <- ofUnit
        d2 <- ofUnit
        _  <- (d1.get >> d2.complete(())).start
        _  <- nap
        _  <- d2.tryGet.map(_ ==> None)
        _  <- d1.complete(())
        _  <- d2.get
      } yield ()
    }

    "TDeferred.complete twice fails" - {
      for {
        d <- TDeferred.in[IO, Unit]
        _ <- d.complete(()).commit[IO]
        r <- d.complete(()).result
      } yield assert(r.is[STMAbort])
    }
  }
}
