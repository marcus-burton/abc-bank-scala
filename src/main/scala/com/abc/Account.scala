package com.abc

import scala.collection.mutable.ListBuffer

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

class Account(val accountType: Int) {
  var balance = 0.0
  val transactions: ListBuffer[Transaction] = ListBuffer()
  val uid =  java.util.UUID.randomUUID.toString
  def sumTransactions = balance

  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      this.synchronized {
        transactions += Transaction(amount)
        balance += amount
      }
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      this.synchronized {
        transactions += Transaction(-amount)
        balance -= amount
      }
  }


  def hasWithdrawWithinDays(days: Int): Boolean = {
     val curDay = DateProvider.now
     def hasWithDrawHelper(days: Int, tlist: List[Transaction]): Boolean = {
       tlist match {
         case Nil => false
         case transaction::xs => if(BankHelper.dayDiff(transaction.transactionDate, curDay) < days && transaction.amount < 0) true
                                      else hasWithDrawHelper(days, xs)
       }
     }
    hasWithDrawHelper(days, transactions.toList)
  }

  def interestEarned: Double = {
    accountType match {
      case Account.SAVINGS =>
        if (balance <= 1000) balance * 0.001
        else 1 + (balance - 1000) * 0.002
      case Account.MAXI_SAVINGS =>
        if (!hasWithdrawWithinDays(10)) return balance * 0.05
        else return balance * 0.001
      case _ =>
        balance * 0.001
    }
  }


}