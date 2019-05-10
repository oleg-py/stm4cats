package com.olegpy.stm.misc

import cats.{Functor, Invariant}
import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.TryableDeferred
import com.olegpy.stm.{STM, TRef}
import cats.implicits._

class TDeferred[A] (private[stm] val state: TRef[Option[A]]) extends TryableDeferred[STM, A] { outer =>
  def tryGet: STM[Option[A]] = state.get
  def get: STM[A] = tryGet.unNone
  def complete(a: A): STM[Unit] = state.updateF {
    case Some(_) => STM.abort(new IllegalStateException("Attempting to complete deferred twice"))
    case None => STM.pure(Some(a))
  }

  // N.B: cannot use this.mapK as that doesn't return TryableDeferred
  def in[F[_]: Concurrent]: TryableDeferred[F, A] = new TryableDeferred[F, A] {
    def tryGet: F[Option[A]] = outer.tryGet.commit[F]
    def get: F[A] = outer.get.commit[F]
    def complete(a: A): F[Unit] = outer.complete(a).commit[F]
  }

  override def toString: String = state.unsafeLastValue match {
    case Some(value) => s"TDeferred(<completed>: $value)"
    case None => s"TDeferred(<not completed>)"
  }
}

object TDeferred {
  def apply[A]: STM[TDeferred[A]] = TRef(Option.empty[A]).map(new TDeferred(_))
  def in[F[_]: Sync, A]: F[TDeferred[A]] = STM.unsafeToSync(apply)

  implicit val invariant: Invariant[TDeferred] = new Invariant[TDeferred] {
    def imap[A, B](fa: TDeferred[A])(f: A => B)(g: B => A): TDeferred[B] = {
      val fo = Functor[Option]
      new TDeferred[B](fa.state.imap(fo.lift(f))(fo.lift(g)))
    }
  }
}
