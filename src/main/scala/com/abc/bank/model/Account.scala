package com.abc.bank.model

import java.time._

import com.abc.bank.utils.Util._

class Account(val accountType: AccountType) {

  private[model] var transactions: List[Transaction] = Nil

  def deposit(amount: BigDecimal):Either[TransactionError, Transaction] = {
    if (amount <= 0)
      Left("amount must be greater than zero")
    else {
      transactions.synchronized {
        val transaction = Transaction(amount)
        transactions = transaction :: transactions
        Right(transaction)
      }
    }
  }

  def withdraw(amount: BigDecimal):Either[TransactionError, Transaction] = {
    if (amount <= 0)
      Left("amount must be greater than zero")
    else {
      transactions.synchronized {
        val transaction = Transaction(-amount)
        transactions = transaction :: transactions
        Right(transaction)
      }
    }
  }

  def rollBackTransaction(transaction: Transaction): Either[TransactionError, Success] = {
      if(transactions.contains(transaction)) {
        transactions.synchronized {
          transactions = transactions.filter(_ != transaction)
        }
        Right()
      } else {
        Left(s" Transaction: ${transaction.toString} does not exists")
    }
  }

  def interestEarned: BigDecimal = {
    val amount: BigDecimal = sumTransactions()
    accountType match {
      case Savings =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case MaxiSavings =>
        val delay = LocalDate.now().minusDays(10)
        if (transactions.takeWhile({ item =>
          val instant = Instant.ofEpochMilli(item.transactionDate.getTime)
          LocalDateTime.ofInstant(instant, ZoneId.systemDefault()).toLocalDate.isAfter(delay)
        }).nonEmpty) {
          amount * 0.01
        } else {
          amount * 0.05
        }
      case Checking =>
        amount * 0.001
    }
  }

  def dailyInterest(days:Int):BigDecimal = {
    interestEarned / days
  }

  def addCurrentInterest:Either[AccountError, Success] = {
    val amount = dailyInterest(Year.now().length()) * sumTransactions() setScale(2, BigDecimal.RoundingMode.HALF_UP)
    deposit(amount) match {
      case Left(error) => Left("Adding Current Interest Error: "+error)
      case Right(transaction) => Right()
    }
  }

  def sumTransactions(): BigDecimal = transactions.map(_.amount).sum

  def statement: String = {
    val transactionSummary = transactions.map(t => withdrawalOrDepositText(t) + " " + t.amount.abs.toDollars)
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${transactions.map(_.amount).sum.toDollars}"
    accountType.toString + "\n" + transactionSummary + totalSummary
  }

  private def withdrawalOrDepositText(t: Transaction) =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

}