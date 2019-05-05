package com.olegpy.stm

import cats.data.State
import cats.implicits._
import cats.effect.{IO, SyncIO}
import com.olegpy.stm.results._
import utest._


object TRefTests extends TestSuite with BaseIOSuite {
  val tests = Tests {

    "TRef.apply" - {
      TRef(number).flatMap(_.get).result
        .map { _ ==> STMSuccess(number) }
    }

    "TRef.in" - {
      for {
        tr <- TRef.in[SyncIO](number).toIO
        x  <- tr.get.commit[IO]
      } yield assert(x == number)
    }

    "TRef#modifyState" - {
      for {
        tr <- TRef.in[IO](number)
        stateOp = State.get[Int] <* State.set(0)
        n  <- tr.modifyState(stateOp).commit[IO]
        _ = n ==> number
        x  <- tr.get.commit[IO]
      } yield x ==> 0
    }

    "TRef#access" - {
      for {
        tr <- TRef.in[IO](0)
        _  <- tr.access
          .filter { case (i, _) => i == 0 }
          .flatMap { case (_, set) => set(number) >> tr.get }
          .result.map(_ ==> STMSuccess(number))
      } yield ()
    }

    "TRef#tryModifyState, TRef#tryModify and TRef#tryUpdate never fail" - {
      for {
        tr <- TRef.in[IO](number)
        stateOp = State.get[Int] <* State.set(0)
        _ <- (
          tr.tryModifyState(stateOp),
          tr.tryModify(_ => (0, 'a')),
          tr.tryUpdate(_ + 1)
        ).tupled.commit[IO].flatMap {
            case (Some(_), Some(_), true) => IO.unit
            case _ => fail[Unit]
        }
      } yield ()
    }
  }
}
