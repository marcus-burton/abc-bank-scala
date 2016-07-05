package com.abc

import java.time.Instant

object DateProvider {
  def getInstance: DateProvider = {
    if (instance == null) instance = new DateProvider
    instance
  }

  private var instance: DateProvider = null
}

class DateProvider {
    def now: Instant = {
    return Instant.now()
  }
}

