package com.abc

import java.util.Calendar
import java.util.Date
import java.util.GregorianCalendar

object DateProvider {
  val gregCal = new GregorianCalendar()
  def now = Calendar.getInstance.getTime

  def isLeapYear: Boolean = gregCal.isLeapYear(Calendar.getInstance.get(Calendar.YEAR))
}