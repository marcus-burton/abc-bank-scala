package com.abc

import com.abc.Helpers._

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

  def interestEarned = accountType.calculateInterest(sumTransactions, hadActivityInLast10Days)

  def sumTransactions = transactions.map(_.amount).sum

  private def hadActivityInLast10Days: Option[Boolean] = {
    accountType match {
      case MAXI_SAVINGS_PLUS => Some(recentTransactionsOccured(transactions, 10))
      case _ => None
    }
  }

  def statement = accountType + "\n" + transactionSummary + totalSummary

  private def transactionSummary = transactions.map(t => t.transactionType + " " + toDollars(t.amount.abs)).mkString("  ", "\n  ", "\n")

  private def totalSummary = s"Total ${toDollars(transactions.map(_.amount).sum)}"

}

