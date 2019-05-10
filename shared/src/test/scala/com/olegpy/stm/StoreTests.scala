package com.olegpy.stm

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import com.olegpy.stm.internal.Store
import utest._

object StoreTests extends TestSuite {
  val tests = Tests {
    "Store resolves conflicting updates" - {
      val store = Store.forPlatform()
      val key1, key2 = new Object
      store.transact(store.current().update(key1, 0))
      store.transact(store.current().update(key2, 0))
      def increment(key: Object): Unit = store.transact {
        val j = store.current()
        j.update(key, j.read(key).asInstanceOf[Int] + 1)
      }
      val execs = 10000
      Future.sequence { List.tabulate(execs * 2)(i => Future {
        if (i % 2 == 0) increment(key1) else increment(key2)
      }) }
        .map { _ =>
          val (r1, r2) = store.transact {
            val j = store.current()
            (j.read(key1), j.read(key2))
          }
          assert(r1 == execs)
          assert(r2 == execs)
        }
    }
  }
}
