package com.olegpy.stm

class UnexpectedRetryInSyncException extends RuntimeException(
  "Attempt to use STM.retry with STM.unsafeCommitSync"
)
