package com.abc

import java.time.temporal.ChronoUnit
import java.time.Instant

import scala.collection.mutable.ListBuffer

case class InsufficientFundsException(msg: String) extends RuntimeException
case class InvalidAmountException(msg: String) extends RuntimeException

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

case class Account(val accountType: Int, protected val transactions: ListBuffer[Transaction] = ListBuffer()) {
  final val BPS_10_PER_DAY: BigDecimal = 0.1 / 100 / 365

  def checkAmount(amount: BigDecimal): Boolean = {
    amount > 0 && (amount * 100).isWhole()
  }

  def deposit(amount: BigDecimal) = checkAmount(amount) match {
    case false => throw new InvalidAmountException("Can't deposit as amount must be greater than zero and can't have fractions of the cents")
    case true => transactions += Transaction(amount)
  }

  def withdraw(amount: BigDecimal) = checkAmount(amount) match {
    case false => throw new InvalidAmountException("Can't withdraw as amount must be greater than zero and can't have fractions of the cents")
    case true => (this.sumTransactions >= amount) match {
      case true => transactions += Transaction(-amount)
      case false => throw new InsufficientFundsException("Can't withdraw more funds than it is available")
    }
  }

  def transactionAmounts: List[BigDecimal] = transactions.map(_.amount).toList

  def interestEarned: BigDecimal = {
    def withinDays(t: Transaction): Boolean = {
      t.transactionDate.isAfter(Instant.now().truncatedTo(ChronoUnit.DAYS).minus(9, ChronoUnit.DAYS))
    }
    val dateDiffTransactions = transactions.map(t => (java.time.Duration.between(Instant.now(), t.transactionDate).toDays, t.amount))
    val grpByDateTransactions = dateDiffTransactions.groupBy(_._1).map(kv => (kv._1, kv._2.map(_._2).sum))
    val minDate = grpByDateTransactions.minBy(_._1)._1
    val result = for {
      days <- minDate to 0l by 1
      amount = grpByDateTransactions.filter(_._1 <= days).map { case (k, v) => v }.sum
      interest = accountType match {
        case Account.SAVINGS =>
          if (amount <= 1000) amount * BPS_10_PER_DAY
          else (1000 * BPS_10_PER_DAY) + ((amount - 1000) * 2.0 * BPS_10_PER_DAY)
        case Account.MAXI_SAVINGS =>
          if (!transactions.filter(_.amount < 0).exists(withinDays)) amount * 50.0 * BPS_10_PER_DAY
          else amount * BPS_10_PER_DAY
        case _ =>
          amount * BPS_10_PER_DAY
      }
    } yield (days, amount, interest)

    result.map(_._3).sum.setScale(2,scala.math.BigDecimal.RoundingMode.HALF_EVEN)
  }

  def sumTransactions: BigDecimal = transactions.map(_.amount).sum

}