package com.abc

import scala.collection.mutable.ListBuffer

class Account(val accountType: AccountType, transactions_ : ListBuffer[Transaction] = ListBuffer()) {

  def transactions: List[Transaction] = transactions_.toList

  def deposit(amount: Double):Unit = addTransaction(amount, deposit =true)

  def withdraw(amount: Double):Unit = addTransaction(-amount, deposit=false)

  private def addTransaction(amount: Double, deposit: Boolean): Unit = {
    require(if(deposit) amount > 0 else amount < 0, "amount must be greater than zero")
    transactions_ += Transaction(amount)
  }

  def interestEarned: Double = accountType.interestEarned(amount = sumTransactions())

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum
}