package com.olegpy.stm

import cats.effect.Sync
import internal.TRefImpl


trait TRef[A] {
  def get: STM[A]
  def set(a: A): STM[Unit]
}

object TRef {
  def apply[A](initial: A): STM[TRef[A]] = STM.delay(new TRefImpl(initial))
  def in[F[_]]: InPartiallyApplied[F] = new InPartiallyApplied[F]

  final class InPartiallyApplied[F[_]](private val dummy: Boolean = false) extends AnyVal {
    def apply[A](initial: A)(implicit F: Sync[F]): F[TRef[A]] =
      F.delay(new TRefImpl(initial))
  }
}
