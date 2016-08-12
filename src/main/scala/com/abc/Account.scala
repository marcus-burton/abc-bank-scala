package com.abc

import java.util.Date

import scala.collection.mutable.ListBuffer

object Account extends Enumeration {
  val CHECKING, SAVINGS, MAXI_SAVINGS = Value.id
}

class Account(val accountType: Int) {
  private val transactions: ListBuffer[Transaction] = ListBuffer()
  // Summing all transactions would get expensive over time. If we carefully tally a total
  // we can avoid that and use total transaction summing for validation periodically.
  private var totalAmount: Double = 0.0

  def deposit(amount: Double, transactionDate: Date = DateProvider.now) {
    if (amount <= 0)
      throw new IllegalArgumentException("deposit amount must be greater than zero")
    else
      doTransaction(amount, transactionDate)
  }

  def withdraw(amount: Double, transactionDate: Date = DateProvider.now) {
    if (amount <= 0)
      throw new IllegalArgumentException("withdraw amount must be greater than zero")
    else
      doTransaction(-amount, transactionDate)
  }

  def transfer(amount: Double, destinationAccount: Account, transactionDate: Date = DateProvider.now) {
    if (amount <= 0) {
      throw new IllegalArgumentException("transfer amount must be greater than zero")
    } else {
      destinationAccount.deposit(amount)
      this.withdraw(amount)
    }
  }

  def interestEarned: Double = {

    accountType match {
      case Account.SAVINGS =>
        if (totalAmount <= 1000) totalAmount * 0.001
        else 1 + (totalAmount - 1000) * 0.002
      case Account.MAXI_SAVINGS =>
        val tenDaysAgo = DateProvider.getDate(-10)
        val earned = if (transactions.takeWhile(!_.transactionDate.before(tenDaysAgo)).exists(_.amount < 0)) {
          totalAmount * 0.001
        } else {
          totalAmount * 0.05
        }
        earned
      case _ =>
        totalAmount * 0.001
    }
  }

  def balance: Double = totalAmount

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.foldLeft(0.0)(_ + _.amount)

  def getTransactions = transactions.toList

  private def doTransaction(amount: Double, transactionDate: Date) {
    transactions prepend Transaction(amount, transactionDate)
    totalAmount += amount
  }
}