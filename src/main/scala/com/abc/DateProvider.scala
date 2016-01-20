package com.abc

import java.util.Calendar
import java.util.Date


object DateProvider {


  def getInstance: DateProvider = {
    if (instance == null) instance = new DateProvider
    instance
  }

  private var instance: DateProvider = null
}

class DateProvider {

  def now: Date = {
    return Calendar.getInstance.getTime
  }

  def now_minus_10: Date = {
    val cal  = Calendar.getInstance()
    cal.add(Calendar.DATE, -10)
    cal.getTime()
  }

}

