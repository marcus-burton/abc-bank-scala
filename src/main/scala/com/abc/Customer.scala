package com.abc

import java.time.Instant

import scala.collection.mutable.ListBuffer

class Customer(val name: String, var accounts: ListBuffer[Account] = ListBuffer()) {

  def openAccount(account: Account): Customer = {
    accounts += account
    this
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned(atDate: Instant = Instant.now): BigDecimal = accounts.map(_.interestEarned(atDate)).sum

  /**
   * This method gets a statement
   */
  def getStatement: String = {
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions()).sum
    f"""
      |Statement for $name
      |${accounts.map(statementForAccount).mkString("\n", "\n\n", "\n")}
      |Total In All Accounts ${toDollars(totalAcrossAllAccounts)}
    """.stripMargin('|').trim
  }

  private def statementForAccount(a: Account): String = {
    val accountType = a.accountType match {
      case Account.Checking =>
        "Checking Account\n"
      case Account.Savings =>
        "Savings Account\n"
      case Account.MaxiSavings =>
        "Maxi Savings Account\n"
    }
    val transactionSummary = a.transactions.map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(a.transactions.map(_.amount).sum)}"
    accountType + transactionSummary + totalSummary
  }

  private def withdrawalOrDepositText(t: Transaction) =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

  private def toDollars(number: BigDecimal): String = f"$$$number%.2f"
}

