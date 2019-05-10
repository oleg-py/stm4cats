package com.olegpy.stm.misc

import scala.collection.immutable.Queue

import cats.{Foldable, Functor, Invariant}
import cats.effect.Sync
import com.olegpy.stm.{STM, TRef}
import cats.syntax.all._
import cats.instances.option._

trait TQueue[A] {
  def offer(a: A): STM[Boolean]
  def tryPeek: STM[Option[A]]
  protected def drop1 : STM[Unit]

  def tryDequeue: STM[Option[A]] = tryPeek.flatTap {
    case Some(_) => drop1
    case _ => STM.unit
  }

  def isEmpty: STM[Boolean] = tryPeek.map(_.isEmpty)

  def enqueue(a: A): STM[Unit] = offer(a).flatMap(STM.check)
  def enqueueAll[F[_]: Foldable](fa: F[A]): STM[Unit] = fa.traverse_(enqueue)

  def peek: STM[A] = tryPeek.unNone
  def dequeue: STM[A] = tryDequeue.unNone
  def dequeueUpTo(n: Int): STM[List[A]] = STM.suspend {
    val b = List.newBuilder[A]
    def loop(n: Int): STM[List[A]] =
      if (n > 0) (dequeue.map(b += _) >> loop(n - 1)) orElse STM.pure(b.result())
      else STM.pure(b.result())

    if (n < 0) STM.abort(new IllegalArgumentException(s"Cannot dequeue $n elements"))
    else loop(n)
  }

  protected def debugValues: Seq[A]
  protected def debugType: String

  override def toString: String = s"TQueue($debugType)(${debugValues.mkString(",")})"
}

object TQueue {
  def synchronous[A]: STM[TQueue[A]] = TRef(none[A]).map { slot =>
    new TQueue[A] {
      def offer(a: A): STM[Boolean] = slot.get
        .flatMap(_.fold(slot.set(a.some).as(true))(_ => STM.pure(false)))
      def tryPeek: STM[Option[A]] = slot.get

      protected def drop1: STM[Unit] = slot.set(None)

      protected def debugValues: Seq[A] = slot.unsafeLastValue().toSeq
      protected def debugType: String = "synchronous"
    }
  }

  def synchronousIn[F[_]: Sync, A]: F[TQueue[A]] = STM.tryCommitSync(synchronous)

  def unbounded[A]: STM[TQueue[A]] = TRef(Queue.empty[A]).map { state =>
    new TQueue[A] {
      def offer(a: A): STM[Boolean] = state.update(_.enqueue(a)).as(true)
      def tryPeek: STM[Option[A]] = state.get.map(_.headOption)
      protected def drop1: STM[Unit] = state.update(_.drop(1))

      protected def debugValues: Seq[A] = state.unsafeLastValue()
      protected def debugType: String = "unbounded"
    }
  }

  def unboundedIn[F[_]: Sync, A]: F[TQueue[A]] = STM.tryCommitSync(unbounded)

  def bounded[A](max: Int): STM[TQueue[A]] = TRef(Vector.empty[A]).map { state =>
    new TQueue[A] {
      def offer(a: A): STM[Boolean] = state.modify {
        case vec if vec.length < max => (vec :+ a, true)
        case vec => (vec, false)
      }

      def tryPeek: STM[Option[A]] = state.get.map(_.headOption)
      protected def drop1: STM[Unit] = state.update(_.drop(1))

      protected def debugValues: Seq[A] = state.unsafeLastValue()
      protected def debugType: String = s"bounded($max)"
    }
  }

  def boundedIn[F[_]: Sync, A](max: Int): F[TQueue[A]] = STM.tryCommitSync(bounded(max))

  def circularBuffer[A](max: Int): STM[TQueue[A]] = TRef(Vector.empty[A]).map { state =>
    new TQueue[A] {
      def offer(a: A): STM[Boolean] = state.update {
        case v if v.length < max => v :+ a
        case v => v.drop(1) :+ a
      } as true
      def tryPeek: STM[Option[A]] = state.get.map(_.headOption)
      protected def drop1: STM[Unit] = state.update(_.drop(1))

      protected def debugValues: Seq[A] = state.unsafeLastValue()
      protected def debugType: String = s"circularBuffer($max)"

      override def toString: String =
        s"TQueue(circularBuffer($max))(${state.unsafeLastValue().mkString(", ")})"
    }
  }

  def circularBufferIn[F[_]: Sync, A](max: Int): F[TQueue[A]] =
    STM.tryCommitSync(circularBuffer(max))

  implicit val invariant: Invariant[TQueue] = new Invariant[TQueue] {
    def imap[A, B](fa: TQueue[A])(f: A => B)(g: B => A): TQueue[B] = new TQueue[B] {
      private[this] val liftedF = Functor[Option].lift(f)
      def offer(a: B): STM[Boolean] = fa.offer(g(a))
      def tryPeek: STM[Option[B]] = fa.tryPeek.map(liftedF)

      protected def drop1: STM[Unit] = fa.drop1

      protected def debugValues: Seq[B] = fa.debugValues.map(f)
      protected def debugType: String = fa.debugType
    }
  }
}