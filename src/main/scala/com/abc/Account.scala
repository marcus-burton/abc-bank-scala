package com.abc

import scala.collection.mutable.ListBuffer
import java.time.Instant
import java.time.temporal.ChronoUnit

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
  def deposit(amount: Double, transactionDate: Instant) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(amount, transactionDate)
  }
  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount)
  }
 def withdraw(amount: Double, transactionDate: Instant) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount, transactionDate)
  }
  def transfer(to: Account, amount: Double) = synchronized{
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else {
      transactions += Transaction(-amount)
      to.transactions += Transaction(amount)
    }
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
  def totalInterestEarnedAccruedDaily: Double = {

    var accrInt: (Double, Instant, Double) = interestEarnedAccruedDaily(transactions.head)(DateProvider.getInstance.now)(0)
    if (transactions.size > 1) {
      accrInt._1 + (for (i <- 1 until transactions.size) yield {
        accrInt = interestEarnedAccruedDaily(transactions.apply(i))(accrInt._2)(accrInt._3)
        accrInt._1
      }).sum 
    } else {
      accrInt._1
    }
  }

  def interestEarnedAccruedDaily(curTran: Transaction)(prevTranDate: Instant)(amountSum: Double): (Double, Instant, Double) = {

    def withdrawalInTenDays(tranInstant: Instant): Boolean = {
      val current = Instant.now()
      val daysBetween = math.abs(ChronoUnit.DAYS.between(tranInstant, current))
      if (daysBetween <= 10 && curTran.amount < 0) true
      else
        false
    }
    def numDays(tranInstant: Instant, prevInstant: Instant): Long = {
      math.abs(ChronoUnit.DAYS.between(tranInstant, prevInstant))
    }
    val ndays = numDays(curTran.transactionDate, prevTranDate)
    val amount = curTran.amount + amountSum
    if (ndays > 0) {
      accountType match {
        case Account.SAVINGS =>
          if (amount <= 1000) (amount * 0.001 / 365 * ndays, curTran.transactionDate, amount)
          else (1 + (amount - 1000) * 0.002 / 365 * ndays, curTran.transactionDate, amount)
        case Account.MAXI_SAVINGS =>
          if (withdrawalInTenDays(curTran.transactionDate)) {
            (amount * 0.01 / 365 * ndays, curTran.transactionDate, amount)
          } else {
            (amount * 0.05 / 365 * ndays, curTran.transactionDate, amount)
          }
        case _ =>
          (amount * 0.001 / 365 * ndays, curTran.transactionDate, amount)
      }
    } else
      (0.0, curTran.transactionDate, amount)
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum
}