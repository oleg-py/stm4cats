package com.olegpy.stm

import cats.effect.IO
import utest._
import cats.implicits._

object EdgeCasesTests extends TestSuite with BaseIOSuite {
  val tests = Tests {
    "commit fails on exceptions" - ioTest {
      val Dummy = new Exception()
      def crash(): Unit = throw Dummy
      TRef.in[IO](0)
        .mproduct(_.set(5).map(_ => crash()).commit[IO].attempt)
        .flatMap {
          case (tref, Left(Dummy)) => tref.get.commit[IO]
          case _ => fail
        }
        .map { x => assert(x == 0) }
    }
  }
}
