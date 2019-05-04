package com.olegpy.stm.internal

import scala.annotation.tailrec
import scala.collection.JavaConverters._
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
      val id: Long = mkId.getAndIncrement(),
      val uncommitted: ju.HashMap[AnyRef, (Any, Long)] = new ju.HashMap(),
      val reads: ju.HashSet[AnyRef] = new ju.HashSet()
    ) extends Store.Journal {

      def writtenKeys: collection.Set[AnyRef] = uncommitted.keySet().asScala
      def readKeys: collection.Set[AnyRef] = reads.asScala

      def read(k: AnyRef): Any = {
        if (uncommitted.containsKey(k)) uncommitted.get(k)._1
        else {
          reads.add(k)
          committed.get().get(k) match {
            case null => null
            case t => t._1
          }
        }
      }

      def update(k: AnyRef, v: Any): Unit = {
        uncommitted.put(k, (v, id))
        ()
      }

      def copy() =
        new Journal(id, new ju.HashMap(uncommitted), reads)
    }

    final def current(): Journal = journal.get()

    final def transact[A](f: => A): A = {
      @tailrec def reevaluate(): A = {
        val start = committed.get()
        journal.set(new Journal)
        val result = f
        @tailrec def tryConsolidate(): Boolean = {
          val preCommit = committed.get()
          var hasConflict = start ne preCommit
          val j = journal.get()
          if (hasConflict) {
            hasConflict = false
            val ksi = j.reads.iterator()
            while (ksi.hasNext && !hasConflict) {
              val key = ksi.next()
              hasConflict = start.get(key) != preCommit.get(key)
            }
          }
          if (hasConflict) {
            // This might not be hit in a single test run, avoid fluctuating coverage
            // $COVERAGE-OFF$
            false
            // $COVERAGE-ON$
          } else {
            val end = new ju.WeakHashMap[AnyRef, (Any, Long)](preCommit)
            end.putAll(j.uncommitted)
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
  }
}
