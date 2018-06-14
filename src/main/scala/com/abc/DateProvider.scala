package com.abc

import java.time.{LocalDate, Period}

object DateProvider {
  def getInstance: DateProvider = {
    if (instance == null) instance = new DateProvider
    instance
  }

  def daysFrom(from: LocalDate): Int = {
    Period.between(from, LocalDate.now).getDays
  }

  private var instance: DateProvider = null
}

class DateProvider {
  def now: LocalDate = {
    return LocalDate.now
  }
}

