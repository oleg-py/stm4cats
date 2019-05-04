package com.olegpy.stm

import scala.reflect.ClassTag

import cats.effect.{Concurrent, IO}
import cats.implicits._

object results {
  sealed trait STMResult[+A] {
    def is[T](implicit ct: ClassTag[T]): Boolean =
      ct.runtimeClass.isInstance(this)
  }

  case class STMSuccess[+A](value: A) extends STMResult[A]
  case class STMAbort(reason: Throwable) extends STMResult[Nothing]
  case object STMRetry extends STMResult[Nothing]

  implicit class STMOps[+A](private val self: STM[A]) extends AnyVal {
    def result(implicit c: Concurrent[IO]): IO[STMResult[A]] =
      self.map(STMSuccess(_)).orElse(STMRetry.pure[STM])
        .commit[IO].handleError(STMAbort)
  }
}
