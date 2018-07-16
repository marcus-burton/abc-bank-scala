package com.abc.mutable

import com.abc.pure

import java.time.Instant

object SystemInstantProvider extends pure.InstantProvider {
  override def now(): Instant = Instant.now
}
