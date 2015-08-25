package com.abc

import org.joda.time.DateTime

import scala.collection.mutable.ListBuffer

object Helpers {

  def toDollars(number: Double): String = f"$$$number%.2f"

  def plurialize(number: Int, word: String): String = if (number == 1) word else word + "s"

  def recentWithdrawalOccurred(transactions: ListBuffer[Transaction], periodInDays: Integer): Boolean = {
    transactions.map(t => t.transactionType == "withdrawal" && DateProvider.dateOccurredLessThan(t.transactionDate, periodInDays)
    ).find(d => d) match {
      case Some(x) => true
      case None => false
    }
  }

  def getDailyRate(APR: Double, d: DateTime) = APR / daysInYear(d)

  def daysInYear(d: DateTime) = if (d.year.isLeap) 366 else 365

}
