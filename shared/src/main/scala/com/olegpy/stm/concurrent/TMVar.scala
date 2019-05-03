package com.olegpy.stm.concurrent

import cats.effect.{Concurrent, Sync}
import cats.effect.concurrent.MVar
import com.olegpy.stm.{STM, TRef}
import cats.syntax.flatMap._
import cats.syntax.option._

class TMVar[A] (state: TRef[Option[A]]) extends MVar[STM, A] {
  def isEmpty: STM[Boolean] = state.get.map(_.isEmpty)
  def put(a: A): STM[Unit] = state.updOrRetry { case None => a.some }
  def tryPut(a: A): STM[Boolean] = put(a).as(true) orElse STM.pure(false)
  def take: STM[A] = tryTake.unNone
  def tryTake: STM[Option[A]] = isEmpty.ifM(STM.retry, state.getAndSet(None))
  def read: STM[A] = state.get.unNone

  def to[F[_]: Concurrent]: MVar[F, A] = mapK(STM.atomicallyK[F])
}

object TMVar {
  def empty[A]: STM[TMVar[A]] = TRef(none[A]).map(new TMVar(_))
  def apply[A](initial: A): STM[TMVar[A]] = TRef(initial.some).map(new TMVar(_))

  def in[F[_]: Sync, A](initial: A): F[TMVar[A]] = STM.unsafeToSync(TMVar(initial))
  def emptyIn[F[_]: Sync, A]: F[TMVar[A]] = STM.unsafeToSync(TMVar.empty)
}
