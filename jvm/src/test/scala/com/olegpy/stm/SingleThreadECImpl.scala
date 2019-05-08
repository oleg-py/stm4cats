package com.olegpy.stm

import scala.concurrent.ExecutionContext

import java.util.concurrent.Executors


trait SingleThreadECImpl extends SingleThreadEC {
  override val singleThread: ExecutionContext = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
}
