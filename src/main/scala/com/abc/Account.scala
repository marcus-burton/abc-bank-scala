package com.abc

import org.joda.time.{DateTime, Days}

import scala.collection.mutable.ListBuffer

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

class Account(val accountType: Int, var transactions: ListBuffer[Transaction] = ListBuffer()) {

  /**
    *
    * @param amount deposit
    * @param dateOffset  only for testing
    */
  def deposit(amount: Double, dateOffset: Int = 0) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(amount, dateOffset)
  }

  /**
    *
    * @param amount deposit
    * @param dateOffset only for testing
    */
  def withdraw(amount: Double, dateOffset: Int = 0) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount, dateOffset)
  }

  private def dailyInterest(principle: Double, rate: Double): Double = daysAcctOpen * principle * rate / 365

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    val lastTransDate = transactions.last.transactionDate
    val lastTransDep = transactions.last.amount > 0   // if < 0 => withdrawal
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) dailyInterest(amount, 0.001)
        else dailyInterest(1 + (amount - 1000), 0.002)
      case Account.MAXI_SAVINGS =>
        dailyInterest(amount, if (noWithdrawsLast10) 0.05 else 0.001)
      /*  if (amount <= 1000) return amount * 0.02
        if (amount <= 2000) return 20 + (amount - 1000) * 0.05
        70 + (amount - 2000) * 0.1 */
      case _ =>
        dailyInterest(amount, 0.001)
    }
  }

  def noWithdrawsLast10 : Boolean = {
    // find all transactions that were withdrawals in a ten day period from now
    val now = DateTime.now
    val deposits: List[Boolean] = {
      for {
        trans <- transactions
        if Days.daysBetween(now, trans.transactionDate).getDays < 10
      } yield trans.amount > 0.0
    }.toList
    deposits.foldLeft(true)(_ && _)   // one withdrawal will bit and to false
  }

  /**
    * check not implemented - need further definition of how to check
    * @param checkAllTransactions unclear what to check
    * @return
    */
  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

  /**
    * assume that time flows forward
    *
    * @return difference in days for initial transaction to now
    */
  def daysAcctOpen = Days.daysBetween(transactions.last.transactionDate, transactions.head.transactionDate).getDays + 1
}