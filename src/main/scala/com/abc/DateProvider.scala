package com.abc

import java.util.Calendar
import java.util.Date

object DateProvider {
  lazy val calendarInstance = Calendar.getInstance
  def now: Date = calendarInstance.getTime
}


