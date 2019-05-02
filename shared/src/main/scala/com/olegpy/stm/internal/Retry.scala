package com.olegpy.stm.internal

import scala.util.control.NoStackTrace

import com.olegpy.stm.STM


private[stm] case class Retry(next: STM[_]) extends Throwable with NoStackTrace
