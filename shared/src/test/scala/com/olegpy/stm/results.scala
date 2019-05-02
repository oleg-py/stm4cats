package com.olegpy.stm

import cats.effect.{Concurrent, IO}
import cats.implicits._

object results {
  sealed trait STMResult[+A]

  case class STMSuccess[+A](value: A) extends STMResult[A]
  case class STMAbort(reason: Throwable) extends STMResult[Nothing]
  case object STMRetry extends STMResult[Nothing]

  implicit class STMOps[+A](private val self: STM[A]) extends AnyVal {
    def result(implicit c: Concurrent[IO]): IO[STMResult[A]] =
      self.map(STMSuccess(_)).orElse(STMRetry.pure[STM])
        .commit[IO].handleError(STMAbort)
  }
}
