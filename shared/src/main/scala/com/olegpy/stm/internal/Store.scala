package com.olegpy.stm.internal


trait Store {
  def current(): Store.Journal
  def transact[A](f: => A): A
}

object Store extends StorePlatform {
  trait Journal {
    def read(k: AnyRef): Any
    def update(k: AnyRef, v: Any): Unit
  }
}