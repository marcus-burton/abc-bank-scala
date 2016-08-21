package com.boloutaredoubeni.bank

import java.time.Instant

object DateProvider {
  def getInstance = {
    if (instance == null) instance = new DateProvider
    instance
  }

  private var instance: DateProvider = _
}

final class DateProvider {
  def now = Instant.now()
}

