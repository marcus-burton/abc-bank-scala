package com.abc

import scala.collection.mutable.ListBuffer
import org.joda.time.{DateTime, Days}

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
      transactions += Transaction(amount)
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount)
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case Account.MAXI_SAVINGS =>
        numOfDaysSinceLastWithdrawal match {
          case Some(daysInterval) if daysInterval >= 10 => amount * 0.05
          case Some(daysInterval) => amount * 0.001
          case None if numOfDaysSinceFirstDeposit > 10 => amount * 0.05
          case _ => amount * 0.001
        }
        //if (amount <= 1000) return amount * 0.02
        //if (amount <= 2000) return 20 + (amount - 1000) * 0.05
        //70 + (amount - 2000) * 0.1
      case _ =>
        amount * 0.001
    }
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum
  
  private def numOfDaysSinceLastWithdrawal():Option[Int] = {
    transactions.reverse find ( _.amount < 0) match {
      case Some(lastWithdrawTrans) => { val lastWithdrawDateTime = new DateTime(lastWithdrawTrans.transactionDate);
        val currDateTime = new DateTime();
        val daysInterval = Days.daysBetween(currDateTime , lastWithdrawDateTime).getDays();
        Some(daysInterval)
      }
      case None =>{ println("we are in None"); None }
    }
  }

    private def numOfDaysSinceFirstDeposit:Int = {
      val firstdepositDateTime = new DateTime(transactions.head.transactionDate);
      val currDateTime = new DateTime();
      val daysInterval = Days.daysBetween(currDateTime , firstdepositDateTime).getDays();
      daysInterval
    }

}