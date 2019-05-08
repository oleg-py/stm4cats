package com.olegpy.stm

import scala.concurrent.ExecutionContext


trait SingleThreadECImpl extends SingleThreadEC {
  override val singleThread: ExecutionContext = ExecutionContext.global
}