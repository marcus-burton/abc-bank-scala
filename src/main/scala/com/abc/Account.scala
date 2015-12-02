package com.abc

import scala.collection.mutable.ListBuffer

abstract class Account(val transactions: ListBuffer[Transaction] = ListBuffer()) {

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
    val amount: Double = sumTransactions
    calculateInterest(amount)
  }

  def sumTransactions: Double = transactions.map(_.amount).sum

  protected def calculateInterest(amount: Double): Double
}

case class CheckingAccount(override val transactions: ListBuffer[Transaction] = ListBuffer()) extends Account(transactions) {
  def calculateInterest(amount: Double) = amount * 0.001
  override def toString = "Checking Account"
}

case class SavingsAccount(override val transactions: ListBuffer[Transaction] = ListBuffer()) extends Account(transactions) {
  def calculateInterest(amount: Double) = {
    if (amount <= 1000) amount * 0.001
    else 1 + (amount - 1000) * 0.002
  }
  override def toString = "Savings Account"
}

case class MaxiSavingsAccount(override val transactions: ListBuffer[Transaction] = ListBuffer()) extends Account(transactions) {
  def calculateInterest(amount: Double) = {
    if (amount <= 1000) amount * 0.02
    else if (amount <= 2000) 20 + (amount - 1000) * 0.05
    else 70 + (amount - 2000) * 0.1
  }
  override def toString = "Maxi Savings Account"
}