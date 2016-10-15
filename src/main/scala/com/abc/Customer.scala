package com.abc

import scala.collection.mutable.ListBuffer

class Customer(val name: String, accounts: ListBuffer[Account] = ListBuffer()) {

  private def toDollars(number: Double): String = f"$$$number%.2f"

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double = accounts.map(_.interestEarned).sum

  def openAccount(account: Account): Customer = {
    accounts += account
    this
  }

  def statement: String = {
    //JIRA-123 Change by Joe Bloggs 29/7/1988 start
    var statement: String = null //reset statement to null here
    //JIRA-123 Change by Joe Bloggs 29/7/1988 end
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions()).sum
    statement =
      f"""Statement for $name
         |${accounts.map(statementForAccount).mkString("\n", "\n\n", "\n")}
         |Total In All Accounts ${toDollars(totalAcrossAllAccounts)}""".stripMargin
    statement
  }

  private def statementForAccount(account: Account): String = {
    val transactionSummary = account.transactions.map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(account.transactions.map(_.amount).sum)}"
    s"""${account.accountType.label}
       |$transactionSummary$totalSummary""".stripMargin
  }

  private def withdrawalOrDepositText(transaction: Transaction) =
    transaction.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }
}
