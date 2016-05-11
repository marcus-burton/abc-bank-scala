package com.abc

import org.joda.time._

import scala.collection.mutable.ListBuffer

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

class Account(val accountType: Int, var transactions: ListBuffer[Transaction] = ListBuffer()) {

  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(amount, DateTime.now, "deposit")
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount, DateTime.now, "withdrawal")
  }

  def interestRate(principal: Double, interestRate: Double, daysPast: Double): Double = {
    BigDecimal(principal * (interestRate/365) * daysPast).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble //round to nearest hundreth
    //daily interest rate calculator
  }

  def daysSinceAccountOpened = Days.daysBetween(transactions.head.time, transactions.last.time).getDays //get account's age in days

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) interestRate(amount, 0.001, daysSinceAccountOpened)
        else interestRate(1 + (amount - 1000), 0.002, daysSinceAccountOpened)
      case Account.MAXI_SAVINGS =>
        if (amount <= 1000) interestRate(amount, 0.02, daysSinceAccountOpened)
        if (amount <= 2000 && amount > 1000)  interestRate(20 + (amount - 1000), tenDayWithdrawals, daysSinceAccountOpened)
        else interestRate(70 + (amount - 2000), 0.1, daysSinceAccountOpened)
      case _ =>
        interestRate(amount, 0.001, daysSinceAccountOpened)
    }
  }

  def tenDayWithdrawals: Double = { //check to see if withdrawal occurred within 10 day period.  Rate is returned based on this fact.
    transactions.foreach(t =>
      if(Days.daysBetween(DateTime.now, t.time).getDays <= 10 && t.transType == "withdraw") 0.001
    )
    0.05
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

}