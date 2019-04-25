package com.olegpy

import cats.effect.{Concurrent, IO}
import cats.{FunctorFilter, Monad, MonoidK, ~>}
import cats.implicits._
import com.olegpy.stm.internal.{Monitor, Retry, Store}

package object stm {
  type STM[+A] = STM.Of[A]

  object STM {
    type Base = Any { type STM$newtype$ }
    trait Tag extends Any
    type Of[+A] <: Base with Tag

    val retry: STM[Nothing] = delay { throw Retry }
    def check(c: Boolean): STM[Unit] = retry.whenA(c)

    def atomically[F[_]] = new AtomicallyFn[F]

    final class AtomicallyFn[F[_]](private val dummy: Boolean = false) extends AnyVal {
      def apply[A](stm: STM[A])(implicit F: Concurrent[F]): F[A] =
        atomicallyImpl[F, A](stm)
    }

    def atomicallyK[F[_]: Concurrent]: STM ~> F = new (STM ~> F) {
      def apply[A](fa: STM[A]): F[A] = atomicallyImpl[F, A](fa)
    }

    implicit class STMOps[A](private val self: STM[A]) extends AnyVal {
      def commit[F[_] : Concurrent]: F[A] = atomicallyImpl[F, A](self)

      def orElse[B >: A](other: STM[B]): STM[B] = wrap {
        expose[B](self) recoverWith { case Retry => expose[B](other) }
      }

      def withFilter(f: A => Boolean): STM[A] = self.filter(f)
    }

    implicit val monad: Monad[STM] = Monad[IO].asInstanceOf[Monad[STM]]
    implicit val functorFilter: FunctorFilter[STM] = new FunctorFilter[STM] {
      def functor: cats.Functor[STM] = monad
      def mapFilter[A, B](fa: STM[A])(f: A => Option[B]): STM[B] =
        fa.flatMap(f(_).fold[STM[B]](STM.retry)(_.pure[STM]))
    }

    implicit val monoidK: MonoidK[STM] = new MonoidK[STM] {
      def empty[A]: STM[A] = STM.retry
      def combineK[A](a: STM[A], b: STM[A]): STM[A] = a orElse b
    }


    private[this] def wrap[A](io: IO[A]): STM[A] = io.asInstanceOf[STM[A]]
    private[this] def expose[A](stm: STM[_ >: A]): IO[A] = stm.asInstanceOf[IO[A]]

    private[stm] val store: Store = Store.forPlatform()
    private[stm] def delay[A](a: => A): STM[A] = wrap(IO(a))

    private[this] val globalLock = new Monitor

    private[this] def atomicallyImpl[F[_]: Concurrent, A](stm: STM[A]): F[A] =
      Concurrent[F].suspend {
        try {
          val result = store.transact {
            expose[A](stm).unsafeRunSync()
          }
          globalLock.notifyOneF[F] as result
        } catch {
          case Retry => globalLock.waitF[F] >> atomicallyImpl[F, A](stm)
        }
      }
  }
}
