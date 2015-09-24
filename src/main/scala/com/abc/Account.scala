package com.abc

import java.util.ArrayList
import java.util.Collections

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.asScalaIterator

sealed trait Account {
  val transactions = Collections.synchronizedList(new ArrayList[Transaction]())

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

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.synchronized {
    transactions.iterator.map(_.amount).sum
  }

  def transactionSummary(f: Transaction => String,
                         begin: String,
                         sep: String,
                         end: String) = transactions.synchronized {
    transactions.iterator.map(f).mkString(begin, sep, end)
  }
}

case class CheckingAccount() extends Account {
  def interestEarned: Double = sumTransactions() * 0.001
}

case class SavingsAccount() extends Account {
  def interestEarned: Double = {
    val amount = sumTransactions()
    if (amount <= 1000) amount * 0.001
    else 1 + (amount - 1000) * 0.002
  }
}

case class MaxiSavingsAccount() extends Account {
  def interestEarned: Double = {
    val amount = sumTransactions()
    if (amount <= 1000) return amount * 0.02
    if (amount <= 2000) return 20 + (amount - 1000) * 0.05
    70 + (amount - 2000) * 0.1
  }
}
