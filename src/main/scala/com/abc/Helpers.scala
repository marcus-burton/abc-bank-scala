package com.abc

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

}
