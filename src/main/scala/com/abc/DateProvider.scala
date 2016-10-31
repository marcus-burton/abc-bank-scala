package com.abc

import java.util.Calendar
import java.util.Date

object DateProvider {
  lazy val instance: DateProvider = new DateProvider
}

class DateProvider private () {
  def now: Date = Calendar.getInstance.getTime
}

