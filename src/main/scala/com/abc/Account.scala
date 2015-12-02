package com.abc

import scala.collection.mutable.ListBuffer
import java.util.Calendar
import java.util.Date

abstract class Account(val transactions: ListBuffer[Transaction] = ListBuffer()) {

  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += new RealTransaction(amount)
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += new RealTransaction(-amount)
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions
    calculateInterest(amount)
  }

  def addTransaction(t: Transaction): Unit = transactions += t

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
    /**
    if (amount <= 1000) amount * 0.02
    else if (amount <= 2000) 20 + (amount - 1000) * 0.05
    else 70 + (amount - 2000) * 0.1
    */
    if ( hasWithdrawalInPastTenDays )
      amount * 0.001
    else
      amount * 0.05
  }

  override def toString = "Maxi Savings Account"

  def hasWithdrawalInPastTenDays: Boolean = {
    val now = DateProvider.now

    val cal = Calendar.getInstance()
    cal.setTime(now)
    cal.add(Calendar.DATE, -10)
    val tenDaysAgo = cal.getTime

    transactions.exists( t => t.transactionDate.after(tenDaysAgo) && t.amount < 0.0 )
  }
}