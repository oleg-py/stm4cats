package com.olegpy.stm.concurrent

import scala.collection.immutable.Queue

import cats.Foldable
import cats.data.{Chain, NonEmptyChain}
import cats.effect.Sync
import com.olegpy.stm.{STM, TRef}
import cats.syntax.all._

trait TQueue[A] {
  def offer(a: A): STM[Boolean]
  def tryDequeue: STM[Option[A]]

  def enqueue(a: A): STM[Unit] = offer(a).flatMap(STM.check)
  def enqueueAll[F[_]: Foldable](fa: F[A]): STM[Unit] = fa.traverse_(enqueue)

  def dequeue: STM[A] = tryDequeue.unNone
  def dequeueUpTo(n: Int): STM[NonEmptyChain[A]] = {
    def loop(as: Chain[A]): STM[Chain[A]] = tryDequeue.flatMap {
      case Some(a) => loop(as :+ a)
      case None => STM.pure(as)
    }
    loop(Chain.empty).mapFilter(NonEmptyChain.fromChain)
  }

}

object TQueue {
  def synchronous[A]: STM[TQueue[A]] = TRef(none[A]).map { slot =>
    new TQueue[A] {
      def offer(a: A): STM[Boolean] = slot.get
        .flatMap(_.fold(slot.set(a.some).as(true))(_ => STM.pure(false)))
      def tryDequeue: STM[Option[A]] = slot.get
    }
  }

  def synchronousIn[F[_]: Sync, A]: F[TQueue[A]] = STM.unsafeToSync(synchronous)

  def unbounded[A]: STM[TQueue[A]] = TRef(Queue.empty[A]).map { state =>
    new TQueue[A] {
      def offer(a: A): STM[Boolean] = state.update(_.enqueue(a)).as(true)

      def tryDequeue: STM[Option[A]] = state.modify { q =>
        q.dequeueOption match {
          case None => (q, None)
          case Some((hd, rest)) => (rest, hd.some)
        }
      }
    }
  }

  def unboundedIn[F[_]: Sync, A]: F[TQueue[Nothing]] = STM.unsafeToSync(unbounded)

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
    }
  }

  def boundedIn[F[_]: Sync, A](max: Int): F[TQueue[A]] = STM.unsafeToSync(bounded(max))

  def circularBuffer[A](max: Int): STM[TQueue[A]] = TRef(Vector.empty[A]).map { state =>
    new TQueue[A] {
      def offer(a: A): STM[Boolean] = state.update { _.take(max - 1) :+ a } as true
      def tryDequeue: STM[Option[A]] = state.modify {
        case hd +: tail => (tail, hd.some)
        case empty => (empty, none)
      }
    }
  }

  def circularBufferIn[F[_]: Sync, A](max: Int): F[TQueue[A]] =
    STM.unsafeToSync(circularBuffer(max))
}