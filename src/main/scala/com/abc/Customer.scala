package com.abc

import scala.collection.mutable.ListBuffer

class Customer(val name: String, var accounts: ListBuffer[Account] = ListBuffer()) {

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
    //JIRA-123 Change by Joe Bloggs 29/7/1988 start
    //var statement: String = null //reset statement to null here
    //JIRA-123 Change by Joe Bloggs 29/7/1988 end
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions()).sum
    val statement = f"Statement for $name\n" +
      accounts.map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
    statement
  }

  private def statementForAccount(a: Account): String = {
    val accountType = a.accountType match {
      case Account.CHECKING =>
        "Checking Account\n"
      case Account.SAVINGS =>
        "Savings Account\n"
      case Account.MAXI_SAVINGS =>
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

  private def toDollars(number: Double): String = f"$$$number%.2f"

  /**
    * simple transfer assume no restriction other than a contains funds
    * @param a from acct
    * @param b to acct
    * @param amt how much to move
    */
  def transfer(a: Account, b: Account, amt: Double): Boolean = {
    if (a.sumTransactions() >= amt) {
      a.withdraw(amt)
      b.deposit(amt)
      true
    } else
      false
  }
}

