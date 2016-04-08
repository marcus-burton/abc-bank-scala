package com.abc

import scala.collection.mutable.ListBuffer
import org.joda.time.DateTime

sealed trait Account {
  val id: String
  val transactions: ListBuffer[Transaction] = ListBuffer()

  def deposit(amount: Double): Unit =
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else {
      val now = DateTime.now
      transactions += Transaction(amount, now)
    }
  def withdraw(amount: Double): Unit =
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else {
      val now = DateTime.now
      transactions += Transaction(-amount, now)
    }

  /**
   * Transfer `amount` to `other`
   */
  def transferTo(amount: Double, other: Account): Unit = {
    this.withdraw(amount)
    other.deposit(amount)
  }

  def interestEarned: Double = {
    val amount = sumTransactions()
    this match {
      case CheckingAccount(_) => amount * 0.001
      case SavingsAccount(_) =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case mx: MaxiSavingsAccount =>
        val rate =
          if (mx.withdrewInLast10Days) 0.001
          else 0.05

        amount * rate
    }
  }

  def sumTransactions(): Double = transactions.map(_.amount).sum

  // For tests
  private[abc] def addTransaction(t: Transaction): Unit =
    transactions += t
}

case class CheckingAccount(val id: String) extends Account
case class SavingsAccount(val id: String) extends Account
case class MaxiSavingsAccount(val id: String) extends Account {
  def withdrewInLast10Days: Boolean = {
    val tenDaysAgo = DateTime.now.minusDays(10)
    transactions
      .find(t => t.amount < 0 && t.transactionDate.isAfter(tenDaysAgo))
      .isDefined
  }
}
