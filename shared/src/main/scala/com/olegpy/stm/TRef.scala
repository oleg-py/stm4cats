package com.olegpy.stm

import cats.InvariantMonoidal
import cats.effect.Sync
import internal.TRefImpl
import cats.implicits._

trait TRef[A] {
  def get: STM[A]
  def set(a: A): STM[Unit]
  def modify(f: A => A): STM[Unit] = get >>= (f >>> set)
  def modifyF(f: A => STM[A]): STM[Unit] = get >>= f >>= set
  def modOrRetry(f: PartialFunction[A, A]): STM[Unit] =
    get.collect(f) >>= set
}

object TRef {
  def apply[A](initial: A): STM[TRef[A]] = STM.delay(new TRefImpl(initial))
  def in[F[_]]: InPartiallyApplied[F] = new InPartiallyApplied[F]

  final class InPartiallyApplied[F[_]](private val dummy: Boolean = false) extends AnyVal {
    def apply[A](initial: A)(implicit F: Sync[F]): F[TRef[A]] =
      F.delay(STM.store.transact { new TRefImpl(initial) })
  }

  implicit val invariantMonoidal: InvariantMonoidal[TRef] = new InvariantMonoidal[TRef] {
    val unit: TRef[Unit] = new TRef[Unit] {
      def get: STM[Unit] = STM.unit
      def set(a: Unit): STM[Unit] = STM.unit
    }

    def imap[A, B](fa: TRef[A])(f: A => B)(g: B => A): TRef[B] = new TRef[B] {
      def get: STM[B] = fa.get map f
      def set(a: B): STM[Unit] = fa.set(g(a))
    }

    def product[A, B](fa: TRef[A], fb: TRef[B]): TRef[(A, B)] = new TRef[(A, B)] {
      def get: STM[(A, B)] = fa.get product fb.get
      def set(a: (A, B)): STM[Unit] = fa.set(a._1) *> fb.set(a._2)
    }
  }
}
