package com.abc

import com.abc.Helpers._
import org.joda.time.DateTime

import scala.collection.mutable.ListBuffer

class Account(val accountType: AccountType, var transactions: ListBuffer[Transaction] = ListBuffer()) {


  def transferTo(to: Account, amount: Double) {
    withdraw(amount)
    to.deposit(amount)
  }

  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += TransactionImpl(amount)
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else if (amount >= sumTransactions)
      throw new UnsupportedOperationException("unsufficient funds")
    else
      transactions += TransactionImpl(-amount)
  }

  def interestEarned = accountType.calculateInterest(sumTransactions, recentWithdrawal)

  private def recentWithdrawal: Option[Boolean] = {
    accountType match {
      case MAXI_SAVINGS_PLUS => Some(recentWithdrawalOccurred(transactions, 10))
      case _ => None
    }
  }

  def statement = accountType + "\n" + transactionSummary + totalSummary

  private def transactionSummary = transactions.map(t => t.transactionType + " " + toDollars(t.amount.abs)).mkString("  ", "\n  ", "\n")

  private def totalSummary = s"Total ${toDollars(sumTransactions)}"

  def sumTransactions = transactions.map(_.amount).sum

  def calculateInterestWithDailyAccrual: Double = {

    var date = new DateTime(transactions.head.transactionDate)
    var balanceAcc = 0d
    var transactionsAcc = 0d

    while (date.isBeforeNow()) {
      val transactionAmount = transactions.filter(t => t.transactionDate == date.toDate).map(_.amount).sum
      transactionsAcc += transactionAmount
      balanceAcc += transactionAmount
      balanceAcc = balanceAcc + (balanceAcc * getDailyRate(0.001, date))
      date = date.plusDays(1)
    }

    balanceAcc - transactionsAcc

  }
}

