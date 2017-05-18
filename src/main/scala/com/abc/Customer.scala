package com.abc

import scala.collection.mutable.ListBuffer

case class Customer(name: String, accounts: ListBuffer[Account] = ListBuffer()) {
  def openAccount(account: Account): Customer = {
    accounts += account
    this
  }

  def numberOfAccounts: Int = accounts.size
  def totalInterestEarned: Double = accounts.map(_.interestEarned).sum

  def getStatement: String = {
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions).sum
    f"Statement for $name\n" +
      accounts.map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
  }

  private[this] def statementForAccount(a: Account): String = {
    val transactionSummary = a.transactions.map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(a.transactions.map(_.amount).sum)}"
    a.accountType.getOrElse("Unknown Account Type\n") + transactionSummary + totalSummary
  }

  private[this] def withdrawalOrDepositText(t: Transaction) =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

  private[this] def toDollars(number: Double): String = f"$$$number%.2f"
}

