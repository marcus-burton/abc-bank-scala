package com.abc

import java.time._

import scala.collection.mutable.ListBuffer

sealed trait AccountType
object Account {
  case object Checking extends AccountType
  case object Savings extends AccountType
  case object MaxiSavings extends AccountType
}

class Account(val accountType: AccountType, var transactions: ListBuffer[Transaction] = ListBuffer()) {

  def transferTo(target: Account, amount: Double): Unit = {
    withdraw(amount)
    target.deposit(amount)
  }

  def deposit(amount: BigDecimal) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(amount)
  }

  def withdraw(amount: BigDecimal) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else if (amount > sumTransactions())
      // It could make sense to allow negative balance,
      // but since it's not specified how it would be
      // supported concerning interest, I choose
      // to forbid it
      throw new IllegalArgumentException("amount cannot be greater than the balance")
    else
      transactions += Transaction(-amount)
  }

  private def computeWithInterests(days: Long, amount: BigDecimal): BigDecimal = {
    def dailyRate(annualRate: Double) = 1 + annualRate / 365

    accountType match {
      case Account.Checking =>
        (1L to days).foldLeft(amount) { (acc, _) =>
          acc * dailyRate(0.001)
        }
      case Account.Savings =>
        (1L to days).foldLeft(amount) { (acc, _) =>
          val base = acc min 1000.0
          val extra = (acc - 1000.0) max 0.0
          base * dailyRate(0.001) + extra * dailyRate(0.002)
        }
      case Account.MaxiSavings =>
        (1L to days).foldLeft(amount) { (acc, elapsed) =>
          if (elapsed < 10) acc * dailyRate(0.001)
          else acc * dailyRate(0.05)
        }
    }
  }

  def balance(atDate: Instant): BigDecimal = {
    // We calculate the interest at midnight of the server's TZ
    def toEpochDays(instant: Instant) = instant.atZone(ZoneId.systemDefault()).toLocalDate.toEpochDay

    val init = transactionsAt(atDate)

    if (init.isEmpty) 0.0
    else {
      val ends = init.tail.toList.map(_.transactionDate) ::: List(atDate)

      val total = (init zip ends).foldLeft(BigDecimal(0)) {
        case (sum, (tx, end)) =>
          val days = toEpochDays(end) - toEpochDays(tx.transactionDate)
          computeWithInterests(days, sum + tx.amount)
      }
      total
    }
  }

  def interestEarned(atDate: Instant = Instant.now): BigDecimal = {
    balance(atDate) - transactionsAt(atDate).map(_.amount).sum
  }

  def transactionsAt(atDate: Instant) = transactions.takeWhile(_.transactionDate.isBefore(atDate))

  def sumTransactions(): BigDecimal = transactions.map(_.amount).sum

}