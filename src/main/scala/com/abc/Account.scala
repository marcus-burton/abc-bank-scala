package com.abc

import scala.collection.mutable.ListBuffer

class Account(val accountType: AccountType, var transactions: ListBuffer[Transaction] = ListBuffer()) {

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

  def transfer(t: Account, amount: Double) {
    withdraw(amount)
    t.deposit(amount)
  }

  def interestEarned: Double = {
    val firstTrans = transactions(0)
    val days = DateProvider.getInstance.daysFromNow(firstTrans)
    val interest = accountType.interestCalc(sumTransactions(), days, hadRecentTrans)
    roundAt(2)(interest)
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

  private def hadRecentTrans: Some[Boolean] = {
     accountType match {
      case MaxiSavingsPlus => Some(hadRecentWithdrawal)
      case _ => Some(false)
    }
  }

  private def hadRecentWithdrawal: Boolean = {
    for (transaction <- transactions) {
      if (transaction.transactionType == "withdrawal" && DateProvider.getInstance.recentWithdrawal(transaction)) return true
    }
    false
  }

  private def roundAt(digit: Int)(num: Double): Double = {
    val s = math pow (10, digit)
    (math round num * s) / s
  }


  override def toString() = accountType.toString

}