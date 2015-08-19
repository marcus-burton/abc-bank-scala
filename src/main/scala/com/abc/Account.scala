package com.abc

import scala.collection.mutable.ListBuffer
import java.util.Calendar
import java.util.Date

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

class Account(val accountType: Int, val customerName: String, var transactions: ListBuffer[Transaction] = ListBuffer()) {

  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(amount)
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount)
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case Account.MAXI_SAVINGS =>
        val currdate = DateProvider.getInstance.now;
        val cutoffDate = new Date(currdate.getTime() - (10 * 24 * 3600 *1000))
        if(transactions.exists(x => x.transactionDate.after(cutoffDate) && x.amount < 0))
          return amount * 0.001
        else
          return amount * 0.05
      case _ =>
        amount * 0.001
    }
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

}