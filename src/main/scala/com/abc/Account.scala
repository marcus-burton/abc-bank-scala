package com.abc

import scala.collection.mutable.ListBuffer

object Account extends Enumeration {
  val CHECKING, SAVINGS, MAXI_SAVINGS = Value.id
}

class Account(val accountType: Int, val transactions: ListBuffer[Transaction] = ListBuffer()) {

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
        if (amount <= 1000) return amount * 0.02
        if (amount <= 2000) return 20 + (amount - 1000) * 0.05
        70 + (amount - 2000) * 0.1
      case _ =>
        amount * 0.001
    }
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.foldLeft(0.0)(_ + _.amount)
}