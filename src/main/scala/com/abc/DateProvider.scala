package com.abc

import java.util.{Calendar, Date}

import org.joda.time.{DateTime, Days}


object DateProvider {

  private lazy val instance = new DateProvider

  def getInstance = instance

  def dateOccurredLessThan(startDate: Date, numberOfDays: Int) = Days.daysBetween(new DateTime(startDate), DateTime.now).getDays < numberOfDays

}

class DateProvider {
  def now = Calendar.getInstance.getTime
}


