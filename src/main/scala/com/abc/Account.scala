package com.abc

import scala.collection.mutable.ListBuffer

//TODO add logging and monadify the code

sealed trait CurrencyTransaction {
  val amount:Double = 0
}
/** Deposit given amount; it is acceptable to deposit a negative amount: this results in a withdrawal */
case class Deposit(override val amount:Double) extends CurrencyTransaction
/** Withdraw given amount; it is acceptable to withdraw a negative amount: this results in a deposit */
case class Withdraw(value:Double) extends CurrencyTransaction {
  override val amount = -value
}

trait Account {
  var transactions: ListBuffer[Transaction] = ListBuffer()
  val accountType:Option[String] = None

  def performTransaction(currencyTransaction: CurrencyTransaction):ListBuffer[Transaction] = transactions += Transaction(currencyTransaction.amount)
  def interestEarned: Double
  def sumTransactions: Double = transactions.map(_.amount).sum
}

class CheckingAccount extends Account {
  override val accountType = Some("Checking Account\n")
  def interestEarned: Double = sumTransactions* 0.001
}

class SavingsAccount extends Account {
  override val accountType = Some("Savings Account\n")
  def interestEarned: Double = {
    val amount: Double = sumTransactions
    if (amount <= 1000) amount * 0.001
    else 1 + (amount - 1000) * 0.002
  }
}

class MaxiSavingsAccount extends Account {
  override val accountType = Some("Maxi Savings Account\n")
  def interestEarned: Double = {
    val amount: Double = sumTransactions
    if (amount <= 1000) return amount * 0.02
    if (amount <= 2000) return 20 + (amount - 1000) * 0.05
    70 + (amount - 2000) * 0.1
  }
}