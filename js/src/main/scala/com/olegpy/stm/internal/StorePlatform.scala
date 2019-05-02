package com.olegpy.stm.internal

import scala.collection.mutable
import scala.scalajs.js


trait StorePlatform {
  def forPlatform(): Store = new Store {
    import scala.scalajs.js.DynamicImplicits._
    @inline implicit def any2JSAny(x: Any): js.Any = x.asInstanceOf[js.Any]
    private[this] val committed =
      js.Dynamic.newInstance(js.Dynamic.global.WeakMap)()

    class Journal extends Store.Journal {
      val readKeys: mutable.Set[AnyRef] = mutable.Set()

      def writtenKeys: collection.Set[AnyRef] = {
        val mSet = mutable.Set[AnyRef]()
        val it = uncommitted.keys().asInstanceOf[js.Iterable[AnyRef]].jsIterator()
        var entry = it.next()
        while (!entry.done) {
          mSet += entry.value
          entry = it.next()
        }
        mSet
      }

      val uncommitted : js.Dynamic =
        js.Dynamic.newInstance(js.Dynamic.global.Map)()

      def read(k: AnyRef): Any = {
        if (uncommitted.has(k)) uncommitted.get(k)
        else {
          readKeys += k
          committed.get(k)
        }
      }

      def update(k: AnyRef, v: Any): Unit = {
        uncommitted.set(k, v)
        ()
      }

      def clear(): Unit = {
        uncommitted.clear()
        ()
      }
    }

    private[this] var theLog: Journal = _
    def current(): Store.Journal = theLog
    def transact[A](f: => A): A = {
      theLog = new Journal
      val result = f
      val it = theLog.uncommitted.asInstanceOf[js.Iterable[js.Array[js.Any]]].jsIterator()
      var entry = it.next()
      while (!entry.done) {
        val arr = entry.value
        committed.set(arr(0), arr(1))
        entry = it.next()
      }
      theLog = null
      result
    }
  }
}
