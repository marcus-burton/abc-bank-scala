package com.abc

import java.util.Calendar
import java.util.Date

object DateProvider {
  def now = Calendar.getInstance.getTime
  def getDays(numDays: Int) = Calendar.getInstance.add(Calendar.DAY_OF_MONTH, 1 * numDays).getTime
}


