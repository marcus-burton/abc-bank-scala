package com.abc.pure

import java.time.Instant

trait InstantProvider {
  def now: Instant
}
