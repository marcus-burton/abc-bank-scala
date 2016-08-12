package com.abc

import java.util.{Calendar, Date, GregorianCalendar}

object DateProvider {
  def now: Date = {
    Calendar.getInstance.getTime
  }

  def getDate(days: Int): Date = {
    val currentDate = DateProvider.now
    val cal = new GregorianCalendar()
    cal.setTime(currentDate)
    cal.add(Calendar.DAY_OF_MONTH, days)
    cal.getTime
  }
}