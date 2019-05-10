package com.olegpy.stm.misc

import scala.collection.immutable.Queue

import cats.{Foldable, Invariant}
import cats.data.{Nested, NonEmptyList}
import cats.effect.Sync
import com.olegpy.stm.{STM, TRef}
import cats.syntax.all._
import cats.instances.option._

trait TQueue[A] {
  def offer(a: A): STM[Boolean]
  def tryDequeue: STM[Option[A]]

  def enqueue(a: A): STM[Unit] = offer(a).flatMap(STM.check)
  def enqueueAll[F[_]: Foldable](fa: F[A]): STM[Unit] = fa.traverse_(enqueue)

  def dequeue: STM[A] = tryDequeue.unNone
  def dequeueUpTo(n: Int): STM[NonEmptyList[A]] = {
    dequeue.iterateUntilRetry.mapFilter(NonEmptyList.fromList)
  }
}

object TQueue {
  def synchronous[A]: STM[TQueue[A]] = TRef(none[A]).map { slot =>
    new TQueue[A] {
      def offer(a: A): STM[Boolean] = slot.get
        .flatMap(_.fold(slot.set(a.some).as(true))(_ => STM.pure(false)))
      def tryDequeue: STM[Option[A]] = slot.get

      override def toString: String =
        s"TQueue(synchronous)(${slot.unsafeLastValue.getOrElse("<empty>")})"
    }
  }

  def synchronousIn[F[_]: Sync, A]: F[TQueue[A]] = STM.tryCommitSync(synchronous)

  def unbounded[A]: STM[TQueue[A]] = TRef(Queue.empty[A]).map { state =>
    new TQueue[A] {
      def offer(a: A): STM[Boolean] = state.update(_.enqueue(a)).as(true)

      def tryDequeue: STM[Option[A]] = state.modify { q =>
        q.dequeueOption match {
          case None => (q, None)
          case Some((hd, rest)) => (rest, hd.some)
        }
      }

      override def toString: String = s"TQueue(unbounded)(${state.unsafeLastValue.mkString(", ")})"
    }
  }

  def unboundedIn[F[_]: Sync, A]: F[TQueue[Nothing]] = STM.tryCommitSync(unbounded)

  def bounded[A](max: Int): STM[TQueue[A]] = TRef(Vector.empty[A]).map { state =>
    new TQueue[A] {
      def offer(a: A): STM[Boolean] = state.modify {
        case vec if vec.length < max => (vec :+ a, true)
        case vec => (vec, false)
      }

      def tryDequeue: STM[Option[A]] = state.modify {
        case hd +: tail => (tail, hd.some)
        case empty => (empty, none)
      }

      override def toString: String = s"TQueue(bounded($max))(${state.unsafeLastValue.mkString(", ")})"
    }
  }

  def boundedIn[F[_]: Sync, A](max: Int): F[TQueue[A]] = STM.tryCommitSync(bounded(max))

  def circularBuffer[A](max: Int): STM[TQueue[A]] = TRef(Vector.empty[A]).map { state =>
    new TQueue[A] {
      def offer(a: A): STM[Boolean] = state.update { _.take(max - 1) :+ a } as true
      def tryDequeue: STM[Option[A]] = state.modify {
        case hd +: tail => (tail, hd.some)
        case empty => (empty, none)
      }

      override def toString: String = s"TQueue(circularBuffer($max))(${state.unsafeLastValue.mkString(", ")})"
    }
  }

  def circularBufferIn[F[_]: Sync, A](max: Int): F[TQueue[A]] =
    STM.tryCommitSync(circularBuffer(max))

  implicit val invariant: Invariant[TQueue] = new Invariant[TQueue] {
    def imap[A, B](fa: TQueue[A])(f: A => B)(g: B => A): TQueue[B] = new TQueue[B] {
      def offer(a: B): STM[Boolean] = fa.offer(g(a))
      def tryDequeue: STM[Option[B]] = Nested(fa.tryDequeue).map(f).value
    }
  }
}