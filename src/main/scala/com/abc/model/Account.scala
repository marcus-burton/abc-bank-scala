package com.abc.model

import java.time._

import com.abc.utils.Util._

class Account(val accountType: AccountType.Value) {

  private var transactions: List[Transaction] = Nil

  def deposit(amount: BigDecimal): Transaction = {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else {
      val transaction = Transaction(amount)
      transactions = transaction :: transactions
      transaction
    }
  }

  def withdraw(amount: BigDecimal): Transaction = {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else {
      val transaction = Transaction(-amount)
      transactions = transaction :: transactions
      transaction
    }
  }

  def rollBackTransaction(transaction: Transaction): Unit = {
    transactions = transactions.filter(_ != transaction)
  }

  def interestEarned: BigDecimal = {
    val amount: BigDecimal = sumTransactions()
    accountType match {
      case AccountType.SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case AccountType.MAXI_SAVINGS =>
        val delay = LocalDate.now().minusDays(10)
        if (transactions.takeWhile({ item =>
          val instant = Instant.ofEpochMilli(item.transactionDate.getTime)
          LocalDateTime.ofInstant(instant, ZoneId.systemDefault()).toLocalDate.isAfter(delay)
        }).nonEmpty) {
          amount * 0.01
        } else {
          amount * 0.05
        }
      case _ =>
        amount * 0.001
    }
  }

  def dailyInterest(days:Int):BigDecimal = {
    interestEarned / days
  }

  def addCurrentInterest = {
    val amount = dailyInterest(Year.now().length()) * sumTransactions() setScale(2, BigDecimal.RoundingMode.HALF_UP)
    deposit(amount)
    amount
  }

  def sumTransactions(): BigDecimal = transactions.map(_.amount).sum

  def statement: String = {
    val accType = accountType match {
      case AccountType.CHECKING =>
        "Checking Account\n"
      case AccountType.SAVINGS =>
        "Savings Account\n"
      case AccountType.MAXI_SAVINGS =>
        "Maxi Savings Account\n"
    }
    val transactionSummary = transactions.map(t => withdrawalOrDepositText(t) + " " + t.amount.abs.toDollars)
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${transactions.map(_.amount).sum.toDollars}"
    accType + transactionSummary + totalSummary
  }

  private def withdrawalOrDepositText(t: Transaction) =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

}