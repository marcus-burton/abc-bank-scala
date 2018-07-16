package com.abc.pure

import java.time.Instant

object EpochInstantProvider extends InstantProvider {
  override val now: Instant = Instant.EPOCH
}
