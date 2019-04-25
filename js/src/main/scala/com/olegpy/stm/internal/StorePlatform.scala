package com.olegpy.stm.internal

import scala.collection.mutable
import scala.scalajs.js


trait StorePlatform {
  def forPlatform(): Store = new Store {
    private[this] val committed =
      js.Dynamic.newInstance(js.Dynamic.global.WeakMap)()

    class Journal extends Store.Journal {
      private[this] def toJS(any: Any): js.Any = any.asInstanceOf[js.Any]
      private[this] val uncommitted = mutable.AnyRefMap[AnyRef, Any]()
      def read(k: AnyRef): Any =
        uncommitted.getOrElse(k, committed.get(toJS(k)))
      def update(k: AnyRef, v: Any): Unit = uncommitted.update(k, v)
      def commitAll(): Unit = uncommitted.foreach {
        case (k, v) => committed.set(toJS(k), toJS(v))
      }
    }

    private[this] var theLog: Journal = _
    def current(): Store.Journal = theLog
    def transact[A](f: => A): A = {
      theLog = new Journal
      val result = f
      theLog.commitAll()
      theLog = null
      result
    }
  }
}
