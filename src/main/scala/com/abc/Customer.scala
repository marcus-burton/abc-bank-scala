package com.abc

import scala.collection.mutable.ListBuffer

class Customer(val name: String, val accounts: ListBuffer[Account] = ListBuffer()) {

  def openAccount(account: Account): Customer = {
    accounts += account
    this
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double = accounts.map(_.interestEarned).sum

  /**
   * This method gets a statement
   */
  def getStatement: String = {
    val totalAcrossAllAccounts: Double = accounts.map(_.sumTransactions).sum
    f"Statement for $name\n" +
      accounts.map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
  }

  private def statementForAccount(a: Account): String = {
    val accountType: String = a match {
      case CheckingAccount() =>
        "Checking Account\n"
      case SavingsAccount() =>
        "Savings Account\n"
      case MaxiSavingsAccount() =>
        "Maxi Savings Account\n"
    }
    val transactionSummary: String =
      a.transactions.map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(a.transactions.map(_.amount).sum)}"
    accountType + transactionSummary + totalSummary
  }

  private def withdrawalOrDepositText(t: Transaction): String =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

  private def toDollars(number: Double): String = f"$$$number%.2f"
}

