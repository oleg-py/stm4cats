package com.olegpy.stm.internal


trait Store {
  def current(): Store.Journal
  def transact[A](f: => A): A
  def attempt[A](f: => A): A

  def unsafeReadCommitted(k: AnyRef): Any
}

object Store extends StorePlatform {
  trait Journal {
    def writtenKeys: collection.Map[AnyRef, Long]
    def readKeys: collection.Map[AnyRef, Long]
    def read(k: AnyRef): Any
    def update(k: AnyRef, v: Any): Unit
  }
}