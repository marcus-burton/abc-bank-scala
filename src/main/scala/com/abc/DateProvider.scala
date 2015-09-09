package com.abc

import java.util.Calendar
import java.util.Date
import org.joda.time.LocalDate
import org.joda.time.Days
object DateProvider {
  def getInstance: DateProvider = {
    synchronized{
    if (instance == null) instance = new DateProvider
    instance
    }
  }

  private var instance: DateProvider = null
}

class DateProvider {
  def now: LocalDate = {
    return new LocalDate()
  }
  def daysBetween(from:LocalDate)=Days.daysBetween(from, new LocalDate()).getDays()
  
}

