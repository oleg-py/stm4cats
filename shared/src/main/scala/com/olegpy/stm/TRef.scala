package com.olegpy.stm

import cats.InvariantMonoidal
import cats.data.State
import cats.effect.Sync
import internal.TRefImpl
import cats.implicits._
import cats.effect.concurrent.Ref

trait TRef[A] extends Ref[STM, A] {
  def get: STM[A]
  def set(a: A): STM[Unit]
  def update(f: A => A): STM[Unit] = get >>= (f >>> set)

  def updateF(f: A => STM[A]): STM[Unit] = get >>= f >>= set

  def updOrRetry(f: PartialFunction[A, A]): STM[Unit] =
    get.collect(f) >>= set

  def getAndSet(a: A): STM[A] = get <* set(a)

  def access: STM[(A, A => STM[Boolean])] = get.tupleRight(set(_).as(true))

  def tryUpdate(f: A => A): STM[Boolean] = update(f).as(true)

  def tryModify[B](f: A => (A, B)): STM[Option[B]] = modify(f).map(_.some)

  def modify[B](f: A => (A, B)): STM[B] = get.map(f).flatMap { case (a, b) => set(a) as b }

  def tryModifyState[B](state: State[A, B]): STM[Option[B]] = modifyState(state).map(_.some)

  def modifyState[B](state: State[A, B]): STM[B] = modify(state.run(_).value)

  override def toString: String = s"TRef($unsafeLastValue)"

  def unsafeLastValue(): A
}

object TRef {
  def apply[A](initial: A): STM[TRef[A]] = STM.delay(new TRefImpl(initial))
  def in[F[_]]: InPartiallyApplied[F] = new InPartiallyApplied[F]

  final class InPartiallyApplied[F[_]](private val dummy: Boolean = false) extends AnyVal {
    def apply[A](initial: A)(implicit F: Sync[F]): F[TRef[A]] =
      STM.tryCommitSync(TRef(initial))
  }

  implicit val invariantMonoidal: InvariantMonoidal[TRef] = new InvariantMonoidal[TRef] {
    val unit: TRef[Unit] = new TRef[Unit] {
      def get: STM[Unit] = STM.unit
      def set(a: Unit): STM[Unit] = STM.unit

      def unsafeLastValue(): Unit = ()
    }

    def imap[A, B](fa: TRef[A])(f: A => B)(g: B => A): TRef[B] = new TRef[B] {
      def get: STM[B] = fa.get map f
      def set(a: B): STM[Unit] = fa.set(g(a))

      def unsafeLastValue(): B = f(fa.unsafeLastValue)
    }

    def product[A, B](fa: TRef[A], fb: TRef[B]): TRef[(A, B)] = new TRef[(A, B)] {
      def get: STM[(A, B)] = fa.get product fb.get
      def set(a: (A, B)): STM[Unit] = fa.set(a._1) *> fb.set(a._2)

      def unsafeLastValue(): (A, B) = (fa.unsafeLastValue, fb.unsafeLastValue)
    }
  }
}
