package com.olegpy.stm.internal


trait Store {
  def current(): Store.Journal
  def transact[A](f: => A): A
  def attempt[A](f: => A): A
}

object Store extends StorePlatform {
  trait Journal {
    def writtenKeys: collection.Set[AnyRef]
    def readKeys: collection.Set[AnyRef]
    def read(k: AnyRef): Any
    def update(k: AnyRef, v: Any): Unit
  }
}