package com.abc

import scala.collection.mutable.ListBuffer

sealed trait Account {
  val transactions: ListBuffer[Transaction] = ListBuffer()

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

  def interestEarned: Double

  def sumTransactions: Double = transactions.map(_.amount).sum

}

final case class CheckingAccount() extends Account {
  override def interestEarned: Double = {
    val amount: Double = sumTransactions
    amount * 0.001 // Checking accounts have a flat rate of 0.1%
  }
}

final case class SavingsAccount() extends Account {
  override def interestEarned: Double = {
    val amount: Double = sumTransactions
    if (amount <= 1000) (0.001 * amount) // Savings accounts have a rate of 0.1% for the first $1,000
    else (0.001 * 1000) + 0.002 * (amount - 1000) // then 0.2% for each additional dollar
  }
}

final case class MaxiSavingsAccount() extends Account {
  override def interestEarned: Double = {
    val amount: Double = sumTransactions
    /* Change Maxi-Savings accounts to have an interest rate of 5% assuming no withdrawals in the past 10 days otherwise 0.1% */
    if (amount <= 1000) (0.02 * amount) // Maxi-Savings accounts have a rate of 2% for the first $1,000
    if (amount <= 2000) (0.02 * 1000) + 0.05 * (amount - 1000) // then 5% for the next $1,000
    (0.02 * 1000) + (.05 * 1000) + 0.1 * (amount - 2000) // then 10%
  }
}