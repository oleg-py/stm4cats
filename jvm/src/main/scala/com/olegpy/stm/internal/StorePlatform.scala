package com.olegpy.stm.internal

import scala.annotation.tailrec

import scala.collection.JavaConverters._
import java.{util => ju}
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}


trait StorePlatform {
  def forPlatform(): Store = new Store {
    private[this] val committed =
      new AtomicReference(new ju.WeakHashMap[AnyRef, (Any, Long)]())
    private[this] val mkId = new AtomicLong()
    private[this] val journal = new ThreadLocal[Journal]

    class Journal extends Store.Journal {

      def writtenKeys: collection.Set[AnyRef] = uncommitted.keySet().asScala
      def readKeys: collection.Set[AnyRef] = reads.asScala

      val id: Long = mkId.getAndIncrement()
      val uncommitted = new ju.HashMap[AnyRef, (Any, Long)]()
      val reads = new ju.HashSet[AnyRef]()

      def read(k: AnyRef): Any = {
        reads.add(k)
        if (uncommitted.containsKey(k)) uncommitted.get(k)._1
        else committed.get().get(k) match {
          case null => null
          case t => t._1
        }
      }

      def update(k: AnyRef, v: Any): Unit = {
        uncommitted.put(k, (v, id))
        ()
      }
    }

    final def current(): Journal = journal.get()

    final def transact[A](f: => A): A = {
      @tailrec def reevaluate(): A = {
        val start = committed.get()
        val j = new Journal
        val result = try {
          journal.set(j)
          f
        } finally {
          journal.remove()
        }
        @tailrec def tryConsolidate(): Boolean = {
          val preCommit = committed.get()
          var hasConflict = start ne preCommit
          if (hasConflict) {
            hasConflict = false
            val ksi = j.reads.iterator()
            while (ksi.hasNext && !hasConflict) {
              val key = ksi.next()
              hasConflict = start.get(key) != preCommit.get(key)
            }
          }
          if (hasConflict) {
            false
          } else {
            val end = new ju.WeakHashMap[AnyRef, (Any, Long)](preCommit)
            end.putAll(j.uncommitted)
            committed.compareAndSet(preCommit, end) || tryConsolidate()
          }
        }
        if (tryConsolidate()) {
          result
        } else {
          reevaluate()
        }
      }
      reevaluate()
    }
  }
}
