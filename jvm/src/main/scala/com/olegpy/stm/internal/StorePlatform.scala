package com.olegpy.stm.internal

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.DynamicVariable

import java.util.concurrent.atomic.AtomicReference


trait StorePlatform {
  def forPlatform(): Store = new Store {
    private[this] val committed = new AtomicReference(mutable.WeakHashMap[AnyRef, Any]())
    private[this] val uncommitted = new DynamicVariable(Map.empty[AnyRef, Any])

    private[this] val theLog = new Store.Journal {
      def read(k: AnyRef): Any = uncommitted.value(k)
      def update(k: AnyRef, v: Any): Unit = {
        uncommitted.value = uncommitted.value.updated(k, v)
      }
    }

    final def current(): Store.Journal = theLog
    final def transact[A](f: => A): A = {
      @tailrec def loop(): A = {
        val start = committed.get()
        val (result, finish) = uncommitted.withValue(Map() ++ start) {
          (f, mutable.WeakHashMap(uncommitted.value.toSeq: _*))
        }
        if (committed.compareAndSet(start, finish)) result else loop()
      }
      loop()
    }
  }
}
