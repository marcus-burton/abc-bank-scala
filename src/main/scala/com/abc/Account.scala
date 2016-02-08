package com.abc

import scala.collection.mutable.ListBuffer

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

class Account(val accountType: Int, val transactions: ListBuffer[Transaction] = ListBuffer()) {

  def deposit(amount: BigDecimal) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(amount)
  }

  def withdraw(amount: BigDecimal) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount)
  }

  def interestEarned: BigDecimal = {
    val amount: BigDecimal = sumTransactions()
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case Account.MAXI_SAVINGS =>
        if (amount <= 1000) return amount * 0.02
        if (amount <= 2000) return 20 + (amount - 1000) * 0.05
        70 + (amount - 2000) * 0.1
      case _ =>
        amount * 0.001
    }
  }

  def sumTransactions(checkAllTransactions: Boolean = true): BigDecimal = transactions.map(_.amount).sum

}