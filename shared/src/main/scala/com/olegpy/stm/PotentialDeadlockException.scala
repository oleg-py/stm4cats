package com.olegpy.stm

class PotentialDeadlockException extends RuntimeException(
  "Potential STM deadlock: `retry` is used before any TRef was read"
)
