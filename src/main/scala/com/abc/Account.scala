package com.abc

import scala.collection.mutable.ListBuffer

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

class Account(val accountType: Int, transactions_ : ListBuffer[Transaction] = ListBuffer()) {

  def transactions: List[Transaction] = transactions_.toList

  def deposit(amount: Double):Unit = addTransaction(amount, deposit =true)

  def withdraw(amount: Double):Unit = addTransaction(-amount, deposit=false)

  private def addTransaction(amount: Double, deposit: Boolean): Unit = {
    require(if(deposit) amount > 0 else amount < 0, "amount must be greater than zero")
    transactions_ += Transaction(amount)
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case Account.MAXI_SAVINGS =>
        if (amount <= 1000) return amount * 0.02
        if (amount <= 2000) return 20 + (amount - 1000) * 0.05
        70 + (amount - 2000) * 0.1
      case _ =>
        amount * 0.001
    }
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum
}