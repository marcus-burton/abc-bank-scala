package com.abc

import java.text.SimpleDateFormat
import java.util.concurrent.TimeUnit
import java.util.{Calendar, Date}


// Added a bunch of utility methods
object DateProvider {

  def getInstance: DateProvider = {
    if (instance == null) instance = new DateProvider
    instance
  }

  private var instance: DateProvider = null
}

class DateProvider {
  val defaultDateFormat: String = "yyyyMMdd"

  def now: Date = {
    return Calendar.getInstance.getTime
  }

  def getToday: String = {
    stringFromDate(now)
  }

  def dateFromString(value: String): Date = {
    val format = new SimpleDateFormat(defaultDateFormat)
    format.parse(value)
  }

  def stringFromDate(value: Date): String = {
    val format = new SimpleDateFormat(defaultDateFormat)
    format.format(value)
  }

  def dateDiff(date1: Date): Long = {
    val suppliedDate = Calendar.getInstance()
    TimeUnit.MILLISECONDS.toDays(Math.abs(Calendar.getInstance().getTimeInMillis - suppliedDate.getTimeInMillis))
  }
}

