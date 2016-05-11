package com.abc

import org.joda.time.{Days, DateTime}

case class Transaction(val amount: Double, daysOffSet: Int = 0) {
  val transactionDate = DateTime.now().plusDays(daysOffSet)

  def daysFromNow: Int = Days.daysBetween(transactionDate, DateTime.now()).getDays
}

