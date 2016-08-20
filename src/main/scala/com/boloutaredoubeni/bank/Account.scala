package com.boloutaredoubeni.bank

import scala.collection.mutable.ListBuffer

abstract class Account(val transactions: ListBuffer[Transaction] = ListBuffer()) {
  // TODO: use Monads, Option or Result here
  final def deposit(amount: Double) {
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

  def interestEarned: Double

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum
}

case class CheckingAccount() extends Account {
  override def interestEarned: Double = {
    val amount: Double = sumTransactions()
    amount * 0.001
  }
}

case class SavingsAccount() extends Account {
  override def interestEarned: Double = {
    val amount: Double = sumTransactions()
    if (amount <= 1000) amount * 0.001
    else 1 + (amount - 1000) * 0.002
  }
}

case class MaxiSavingsAccount() extends Account {
  override def interestEarned: Double = {
    val amount: Double = sumTransactions()
    // TODO: rm return
    if (amount <= 1000) return amount * 0.02
    if (amount <= 2000) return 20 + (amount - 1000) * 0.05
    70 + (amount - 2000) * 0.1
  }
}