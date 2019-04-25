package com.olegpy.stm.internal

import scala.util.control.NoStackTrace


private[stm] case object Retry extends Throwable with NoStackTrace
