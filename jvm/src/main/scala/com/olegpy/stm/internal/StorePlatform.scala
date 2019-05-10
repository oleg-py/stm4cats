package com.olegpy.stm.internal

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.control.NonFatal

import java.{util => ju}
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}


trait StorePlatform {
  def forPlatform(): Store = new Store {
    private[this] val committed =
      new AtomicReference(new ju.WeakHashMap[AnyRef, (Any, Long)]())
    private[this] val mkId = new AtomicLong()
    private[this] val journal = new ThreadLocal[Journal]

    class Journal(
      start: ju.WeakHashMap[AnyRef, (Any, Long)],
      val id: Long = mkId.getAndIncrement(),
      val uncommitted: mutable.AnyRefMap[AnyRef, (Any, Long)] = mutable.AnyRefMap.empty,
      val reads: mutable.AnyRefMap[AnyRef, Long] = mutable.AnyRefMap.empty
    ) extends Store.Journal {
      def writtenKeys: collection.Map[AnyRef, Long] = uncommitted.mapValues(_._2)
      def readKeys: mutable.AnyRefMap[AnyRef, Long] = reads

      def read(k: AnyRef): Any = {
        if (uncommitted contains k) uncommitted(k)._1
        else {
          start.get(k) match {
            case null =>
              reads.update(k, Long.MinValue)
              null
            case (value, version) =>
              reads.update(k, version)
              value
          }
        }
      }

      def update(k: AnyRef, v: Any): Unit = {
        uncommitted.update(k, (v, id))
      }

      def copy() =
        new Journal(start, id, uncommitted ++ Map(), reads)
    }

    final def current(): Journal = journal.get()

    final def transact[A](f: => A): A = {
      @tailrec def reevaluate(): A = {
        val start = committed.get()
        journal.set(new Journal(start))
        val result = f
        @tailrec def tryConsolidate(): Boolean = {
          val preCommit = committed.get()
          var hasConflict = start ne preCommit
          val j = journal.get()
          if (hasConflict) {
            hasConflict = false
            val ksi = j.reads.keysIterator
            while (ksi.hasNext && !hasConflict) {
              val key = ksi.next()
              hasConflict = start.get(key) ne preCommit.get(key)
            }
          }
          if (hasConflict) {
            // This might not be hit in a single test run, avoid fluctuating coverage
            // $COVERAGE-OFF$
            false
            // $COVERAGE-ON$
          } else {
            val end = new ju.WeakHashMap[AnyRef, (Any, Long)](preCommit)
            end.putAll(j.uncommitted.asJava)
            committed.compareAndSet(preCommit, end) || tryConsolidate()
          }
        }
        if (tryConsolidate()) {
          result
        } else {
          // Same as above
          // $COVERAGE-OFF$
          reevaluate()
          // $COVERAGE-ON$
        }
      }
      try {
        reevaluate()
      } finally {
        journal.remove()
      }
    }

    def attempt[A](f: => A): A = {
      val j = current()
      try {
        journal.set(j.copy())
        f
      } catch { case NonFatal(ex) =>
        journal.set(j)
        throw ex
      }
    }

    def unsafeReadCommitted(k: AnyRef): Any = committed.get().get(k) match {
      case null => null
      case t => t._1
    }
  }
}
