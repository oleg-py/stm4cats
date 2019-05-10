package com.olegpy.stm.internal

import com.olegpy.stm.{STM, TRef}


private[stm] class TRefImpl[A](initial: A) extends TRef[A] {
  STM.store.current().update(this, initial)
  def get: STM[A] = STM.delay {
    STM.store.current().read(this).asInstanceOf[A]
  }
  def set(a: A): STM[Unit] = STM.delay {
    STM.store.current().update(this, a)
  }

  protected[stm] def unsafeLastValue: A =
    STM.store.unsafeReadCommitted(this).asInstanceOf[A]
}
