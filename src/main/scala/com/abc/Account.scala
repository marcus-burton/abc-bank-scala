package com.abc

import scala.collection.mutable.ListBuffer
import java.time.LocalDate

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
      Transaction(amount) +=: transactions // head of the list is the latest
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else if (amount > sumTransactions()) 
      throw new IllegalArgumentException("amount cannot be greate than what the account owns")
    else
      Transaction(-amount) +=: transactions // head of the list is the latest
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    val latest: Transaction = transactions.head // Most recent transaction is the last one in the list
    val duration: Int = DateProvider.durationFrom(latest.transactionDate)

    val last = lastWithdrawDate()
    val durationWithdraw: Int = DateProvider.durationFrom(last)

    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case Account.MAXI_SAVINGS => durationWithdraw match {
        case a if a > 10 => amount * 0.05
        case _ => amount * 0.01
      }
      case _ =>
        amount * 0.001
    }

    amount * duration / 365 
  }

  def lastWithdrawDate(): LocalDate = {
    for (t <- transactions) {
      if (t.amount < 0) 
        return t.transactionDate
    }

    return LocalDate.of(0,0,0)
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

}
