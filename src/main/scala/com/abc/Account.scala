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

  def interestEarned: Double = transactions.synchronized {
    transactions.iterator.filter(_.interestAccrural).map(_.amount).sum
  }

  def interestFunction: Double => Double

  def interestEarnedDaily: Double = interestFunction(sumTransactions()) / (if (DateProvider.isLeapYear) 366 else 365)

  def accrueInterst: Unit = transactions += Transaction(interestEarnedDaily, true)

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
  def interestFunction = { amount: Double => 0.001 * amount }
}

case class SavingsAccount() extends Account {
  def interestFunction = { amount: Double =>
    if (amount <= 1000) amount * 0.001
    else 1 + (amount - 1000) * 0.002
  }
}

case class MaxiSavingsAccount() extends Account {
  def interestFunction = { amount: Double =>
    if (amount <= 1000) amount * 0.02
    else if (amount <= 2000) 20 + (amount - 1000) * 0.05
    else 70 + (amount - 2000) * 0.1
  }
}
