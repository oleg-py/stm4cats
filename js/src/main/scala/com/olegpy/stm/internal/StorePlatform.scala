package com.olegpy.stm.internal

import scala.collection.mutable
import scala.scalajs.js
import scala.util.control.NonFatal


trait StorePlatform {
  def forPlatform(): Store = new Store {
    import scala.scalajs.js.DynamicImplicits._
    @inline implicit def any2JSAny(x: Any): js.Any = x.asInstanceOf[js.Any]
    private[this] val committed =
      js.Dynamic.newInstance(js.Dynamic.global.WeakMap)()

    class Journal(
      val uncommitted : js.Dynamic = js.Dynamic.newInstance(js.Dynamic.global.Map)(),
      val readKeys: mutable.Set[AnyRef] = mutable.Set()
    ) extends Store.Journal {

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

      def copy(): Journal = new Journal(
        js.Dynamic.newInstance(js.Dynamic.global.Map)(uncommitted),
        readKeys
      )
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

    def attempt[A](f: => A): A = {
      val j = theLog
      try {
        theLog = j.copy()
        f
      } catch { case NonFatal(ex) =>
        theLog = j
        throw ex
      }
    }
  }
}
