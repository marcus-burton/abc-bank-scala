package com.abc

import scala.collection.mutable.ListBuffer

sealed trait AccountType

object AccountType {
  case object Checking extends AccountType
  case object Savings extends AccountType
  case object MaxiSavings extends AccountType
}

trait Account {
  val transactions: ListBuffer[Transaction] = ListBuffer()

  def deposit(amount: Double): Unit =
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(amount)
  def withdraw(amount: Double): Unit =
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount)

  def interestEarned: Double = {
    val amount = sumTransactions()
    this match {
      case CheckingAccount() => amount * 0.001
      case SavingsAccount() =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case MaxiSavingsAccount() =>
        if (amount <= 1000) return amount * 0.02
        if (amount <= 2000) return 20 + (amount - 1000) * 0.05
        70 + (amount - 2000) * 0.1
    }
  }

  def sumTransactions(): Double = transactions.map(_.amount).sum
}

case class CheckingAccount() extends Account
case class SavingsAccount() extends Account
case class MaxiSavingsAccount() extends Account
