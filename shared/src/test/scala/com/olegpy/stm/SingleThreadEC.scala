package com.olegpy.stm

import scala.concurrent.ExecutionContext


trait SingleThreadEC {
  def singleThread: ExecutionContext = sys.error("Not overriden")
}
