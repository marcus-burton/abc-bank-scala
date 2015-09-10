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

  def recentWithdrawal(trans: Transaction): Boolean = {
    val transDate = trans.transactionDate
    withinDays(transDate, 10)
  }

  def daysFromNow(trans: Transaction): Int = {
    val secondsDifference = now.getTime() - trans.transactionDate.getTime()
    (secondsDifference / (1000 * 60 * 60 * 24).toFloat).toInt
  }

  private def withinDays(compareDate: Date, days: Int): Boolean = {
    val daysBefore = new Date(now.getTime() - (days * 24 * 60 * 60 * 1000))
    compareDate.after(daysBefore)
  }

}

