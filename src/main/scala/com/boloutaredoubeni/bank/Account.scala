package com.boloutaredoubeni.bank

import scala.collection.mutable.ListBuffer

import java.util._

object Account {
  def uuid = UUID.randomUUID.toString
}


abstract sealed class Account(val transactions: ListBuffer[Transaction] = ListBuffer()) {
  val id: String = Account.uuid

  final def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Deposit(amount)
  }

  final def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Withdraw(amount)
  }

  def interestEarned: Double

  final def sumTransactions(checkAllTransactions: Boolean = true) = transactions.map(_.amount).sum
}

final case class CheckingAccount() extends Account {
  override def interestEarned = {
    sumTransactions() * 0.001
  }
}

final case class SavingsAccount() extends Account {
  override def interestEarned = {
    val amount: Double = sumTransactions()
    if (amount <= 1000) amount * 0.001
    else 1 + (amount - 1000) * 0.002
  }
}

final case class MaxiSavingsAccount() extends Account {
  override def interestEarned: Double = {
    val amount = sumTransactions()
    if (amount <= 1000)  return amount * 0.02
    if (amount <= 2000)  return 20 + (amount - 1000) * 0.05
    70 + (amount - 2000) * 0.1
  }
}