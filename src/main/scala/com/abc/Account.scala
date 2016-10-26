package com.abc

import scala.collection.mutable.ListBuffer

object Account {
  sealed trait Type // use sealed trait for exhaustivity check
  object CHECKING extends Type
  object SAVINGS extends Type
  object MAXI_SAVINGS extends Type
}

class Account(val accountType: Account.Type, var transactions: ListBuffer[Transaction] = ListBuffer()) {

  def deposit(amount: Double): Unit = {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(amount)
  }

  def withdraw(amount: Double): Unit = {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount)
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    accountType match {
      case Account.CHECKING =>
        amount * 0.001 // Checking accounts have a flat rate of 0.1%
      case Account.SAVINGS =>
        if (amount <= 1000) (0.001 * amount) // Savings accounts have a rate of 0.1% for the first $1,000
        else (0.001 * 1000) + 0.002 * (amount - 1000) // then 0.2% for each additional dollar
      case Account.MAXI_SAVINGS =>
        /* Change Maxi-Savings accounts to have an interest rate of 5% assuming no withdrawals in the past 10 days otherwise 0.1% */
        if (amount <= 1000) (0.02 * amount) // Maxi-Savings accounts have a rate of 2% for the first $1,000
        if (amount <= 2000) (0.02 * 1000) + 0.05 * (amount - 1000) // then 5% for the next $1,000
        (0.02 * 1000) + (.05 * 1000) + 0.1 * (amount - 2000) // then 10%
      // case _ => // No need for _ in exhaustive match
    }
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

}