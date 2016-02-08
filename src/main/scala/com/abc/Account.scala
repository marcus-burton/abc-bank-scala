package com.abc

import scala.collection.mutable.ListBuffer

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2

  final val BPS:BigDecimal = BigDecimal(0.0001)
}

case class InsufficientFundsException(msg:String) extends RuntimeException
case class InvalidAmountException(msg:String) extends RuntimeException

case class Account(val accountType: Int, private val transactions: ListBuffer[Transaction] = ListBuffer()) {
  def checkAmount(amount:BigDecimal):Boolean = {
    amount > 0 && (amount *100).isWhole()
  }

  def deposit(amount: BigDecimal) = checkAmount(amount) match {
    case false => throw new InvalidAmountException("Can't deposit as amount must be greater than zero and can't have fractions of the cents")
    case true => transactions += Transaction(amount)
  }

  def withdraw(amount: BigDecimal) = checkAmount(amount) match {
    case false => throw new InvalidAmountException("Can't withdraw as amount must be greater than zero and can't have fractions of the cents")
    case true => (this.sumTransactions >= amount) match {
        case true => transactions += Transaction(-amount)
        case false => throw new InsufficientFundsException("Can't withdraw more funds than it is available")}
  }

  def transactionAmounts:List[BigDecimal] = transactions.map(_.amount).toList

  def interestEarned: BigDecimal = {
    val amount: BigDecimal = sumTransactions
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case Account.MAXI_SAVINGS =>
        if (amount <= 1000) return amount * 0.02
        else if (amount <= 2000) return 20 + (amount - 1000) * 0.05
        else 70 + (amount - 2000) * 0.1
      case _ =>
        amount * 0.001
    }
  }

  def sumTransactions: BigDecimal = transactions.map(_.amount).sum

}