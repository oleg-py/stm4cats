package com.olegpy.stm.misc

import cats.implicits._
import cats.effect.IO
import cats.effect.concurrent.Deferred
import com.olegpy.stm.results._
import com.olegpy.stm.{DeterministicIOSuite, STM}
import utest._


object TQueueTests extends DeterministicIOSuite {
  val tests = Tests {
    "TQueue.bounded" - {
      val mkQ = TQueue.boundedIn[IO, Int](3)
      "works in FIFO fashion" - mkQ.flatMap(isFifo)
      "doesn't block in inspect methods" - mkQ.flatMap(testNoBlocking)
      "blocks on dequeue when empty" - mkQ.flatMap(blocksOnDequeue)
      "blocks on enqueue when full" - mkQ.flatMap(blocksOnEnqueue)
    }

    "TQueue.unbounded" - {
      val mkQ = TQueue.unboundedIn[IO, Int]
      "works in FIFO fashion" - mkQ.flatMap(isFifo)
      "doesn't block in inspect methods" - mkQ.flatMap(testNoBlocking)
      "blocks on dequeue when empty" - mkQ.flatMap(blocksOnDequeue)
      "never blocks on enqueue" - mkQ.flatMap(doesntBlockOnEnqueue)
    }

    "TQueue.synchronous" - {
      val mkQ = TQueue.synchronousIn[IO, Int]
      "works in FIFO fashion" - mkQ.flatMap(isFifo)
      "doesn't block in inspect methods" - mkQ.flatMap(testNoBlocking)
      "blocks on dequeue when empty" - mkQ.flatMap(blocksOnDequeue)
      "blocks on enqueue when full" - mkQ.flatMap(blocksOnEnqueue)
      "allows single element only" - mkQ.flatMap { queue =>
        queue.enqueueAll(List(number, number)).result
      }.map(_ ==> STMRetry)
    }

    "TQueue.circularBuffer" - {
      val mkQ = TQueue.circularBufferIn[IO, Int](10)
      "works in FIFO fashion" - mkQ.flatMap(isFifo)
      "doesn't block in inspect methods" - mkQ.flatMap(testNoBlocking)
      "blocks on dequeue when empty" - mkQ.flatMap(blocksOnDequeue)
      "never blocks on enqueue" - mkQ.flatMap(doesntBlockOnEnqueue)
      "drops oldest elements on enqueue" - mkQ.flatMap { queue =>
        queue.enqueueAll(List.range(0, 20)).commit[IO] >>
          queue.dequeueUpTo(Int.MaxValue).result.map(_ ==> STMSuccess(List.range(10, 20)))
      }
    }
  }


  private[this] val mkGate = Deferred.tryableUncancelable[IO, Unit]

  private def noBlock(io: STM[Any]): IO[Unit] =
    for {
      gate <- mkGate
      _    <- (io.commit[IO] >> gate.complete(())).start
      _    <- longNap
      rs   <- gate.tryGet
    } yield assert(rs.nonEmpty)

  private def blockUnblock(io: STM[Any], unblock: STM[Any]): IO[Unit] =
    for {
      gate <- mkGate
      _    <- (io.commit[IO] >> gate.complete(())).start
      _    <- longNap
      rs   <- gate.tryGet
      _    = assert(rs.isEmpty)
      _    <- unblock.commit[IO]
      _    <- nap
      rs2  <- gate.tryGet
    } yield assert(rs2.nonEmpty)

  private def testNoBlocking(q: TQueue[Int]) =
    noBlock(q.offer(number)) >>
    noBlock(q.tryPeek) >>
    noBlock(q.tryDequeue) >>
    noBlock(q.isEmpty) >>
    noBlock(q.dequeueUpTo(Int.MaxValue))

  private def blocksOnDequeue(q: TQueue[Int]): IO[Unit] =
    q.dequeueUpTo(Int.MaxValue).commit[IO] >>
      blockUnblock(q.dequeue, q.enqueue(number))

  private def blocksOnEnqueue(q: TQueue[Int]): IO[Unit] =
    q.enqueue(number).iterateUntilRetry.commit[IO] >>
      blockUnblock(q.enqueue(number), q.dequeue)

  private def doesntBlockOnEnqueue(q: TQueue[Int]): IO[Unit] =
    noBlock(q.enqueueAll(List.range(1, 100)))

  private def isFifo(q: TQueue[Int]): IO[Unit] = {
    val expect = List.range(1, 10)
    expect.traverse_(q.enqueue(_).commit[IO]) &>
      q.dequeue.commit[IO].replicateA(expect.length).map { got =>
        assert(got == expect)
      }
  }
}
