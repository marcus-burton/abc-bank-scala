package com.boloutaredoubeni.bank

import java.time.{Duration, Instant}

object DateProvider {
  def getInstance = {
    if (instance == null) instance = new DateProvider
    instance
  }

  private var instance: DateProvider = _

  def daysPassed(before: Long): Long = {
    val now = new DateProvider().now.getEpochSecond
    val diff = before - now
    Duration.ofMillis(diff).toDays
  }
}

final class DateProvider {
  def now = Instant.now()
}

