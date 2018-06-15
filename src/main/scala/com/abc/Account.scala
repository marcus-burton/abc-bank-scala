package com.abc

import scala.collection.mutable.ListBuffer

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

//class Account(val accountType: Int, var transactions: ListBuffer[Transaction] = ListBuffer()) {
class Account(val accountType: Int) {
  var transactions = new ListBuffer[Transaction]
  
  def deposit(amount: Double) {
    if (amount <= 0) throw new IllegalArgumentException("amount must be greater than zero")
    
    transactions += Transaction(amount)
  }

  def withdraw(amount: Double) {
    if (amount <= 0) throw new IllegalArgumentException("amount must be greater than zero")
    
    transactions += Transaction(-amount)
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case Account.MAXI_SAVINGS =>
        if (amount <= 1000) amount * 0.02
        if (amount <= 2000) 20 + (amount - 1000) * 0.05
        70 + (amount - 2000) * 0.1
      case _ =>
        amount * 0.001
    }
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum
  
  def transfer(to: Account, amount: Double) = {
    if(sumTransactions() < amount) throw new IllegalArgumentException("Not enough balance")
    
    withdraw(amount)
    to.deposit(amount)
  }
}