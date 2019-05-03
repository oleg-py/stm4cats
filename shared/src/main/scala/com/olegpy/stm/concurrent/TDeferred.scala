package com.olegpy.stm.concurrent

import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.TryableDeferred
import com.olegpy.stm.{STM, TRef}


class TDeferred[A] (state: TRef[Option[A]]) extends TryableDeferred[STM, A] { outer =>
  def tryGet: STM[Option[A]] = state.get
  def get: STM[A] = tryGet.unNone
  def complete(a: A): STM[Unit] = state.updateF {
    case Some(_) => STM.abort(new IllegalStateException("Attempting to complete deferred twice"))
    case None => STM.pure(Some(a))
  }

  def in[F[_]: Concurrent]: TryableDeferred[F, A] = new TryableDeferred[F, A] {
    def tryGet: F[Option[A]] = outer.tryGet.commit[F]
    def get: F[A] = outer.get.commit[F]
    def complete(a: A): F[Unit] = outer.complete(a).commit[F]
  }
}

object TDeferred {
  def apply[A]: STM[TDeferred[A]] = TRef(Option.empty[A]).map(new TDeferred(_))
  def in[F[_]: Sync, A]: F[TDeferred[A]] = STM.unsafeToSync(apply)
}
