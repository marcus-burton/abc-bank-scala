package com.abc

import scala.collection.mutable.ListBuffer
import org.joda.time._

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

class Account(val accountType: Int, var transactions: ListBuffer[Transaction] = ListBuffer()) {

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
        if (daysSinceWithdraw > 10) amount * 0.05
        else amount * 0.001
      case _ =>
        amount * 0.001
    }
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

  private val lastWithdrawal = new DateTime(transactions.filter(_.amount < 0).map(_.transactionDate).sorted.head)

  private val daysSinceWithdraw = Days.daysBetween(lastWithdrawal.toLocalDate,
    new DateTime(DateProvider.getInstance.now).toLocalDate).getDays
}