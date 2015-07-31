package com.abc

import java.util.Calendar
import java.util.Date

object DateProvider {
  /*
  private var instance: DateProvider = null 
  
  def getInstance: DateProvider = {
    if (instance == null) instance = new DateProvider
    instance
  }
  */
  
  def now: Date = {
    return Calendar.getInstance.getTime
  }  
}

/*
class DateProvider {
  def now: Date = {
    return Calendar.getInstance.getTime
  }
}
*/
