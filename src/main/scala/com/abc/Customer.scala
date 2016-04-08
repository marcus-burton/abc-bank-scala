package com.abc

import scala.collection.mutable.ListBuffer

class Customer(val name: String) {

  val accounts: ListBuffer[Account] = ListBuffer.empty

  def openAccount(account: Account): Customer = {
    if (accounts.find(_.id == account.id).isDefined)
      throw new IllegalArgumentException("Duplicate account id: " + account.id)
    else {
      accounts += account
      this
    }
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double = accounts.map(_.interestEarned).sum

  def transferAmount(amount: Double, accId1: String, accId2: String): Unit = {
    def err(n: String) =
      throw new IllegalArgumentException(s"Account `$n` not found")
    val act1 = accounts.find(_.id == accId1).getOrElse(err(accId1))
    val act2 = accounts.find(_.id == accId2).getOrElse(err(accId2))
    act1.transferTo(amount, act2)
  }

  /**
   * This method gets a statement
   */
  def getStatement: String = {
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions()).sum
    f"Statement for $name\n" +
      accounts.map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
  }

  private def statementForAccount(a: Account): String = {
    val accountType = a match {
      case CheckingAccount(name) =>
        s"Checking Account - $name\n"
      case SavingsAccount(name) =>
        s"Savings Account - $name\n"
      case MaxiSavingsAccount(name) =>
        s"Maxi Savings Account - $name\n"
    }
    val transactionSummary =
      a.transactions
        .map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
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
}

