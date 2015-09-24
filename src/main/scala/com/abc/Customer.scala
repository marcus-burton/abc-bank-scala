package com.abc

import java.util.ArrayList
import java.util.Collections
import java.util.{ List => JList }

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.asScalaIterator

class Customer(val name: String,
               val accounts: JList[Account] = Collections.synchronizedList(new ArrayList[Account]())) {

  def openAccount(account: Account): Customer = {
    accounts += account
    this
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double = accounts.synchronized { accounts.iterator.map(_.interestEarned).sum }

  /**
   * This method gets a statement
   */
  def getStatement: String = {
    val (accountsStr, totalAcrossAllAccounts) = accounts.iterator.foldLeft("", 0.0) {
      case ((accStr, accTotal), account) =>
        (accStr + "\n\n" + statementForAccount(account),
          accTotal + account.sumTransactions())
    }
    f"Statement for $name" + accountsStr + s"\n\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
  }

  private def statementForAccount(a: Account): String = {
    val accountType = a match {
      case CheckingAccount() =>
        "Checking Account\n"
      case SavingsAccount() =>
        "Savings Account\n"
      case MaxiSavingsAccount() =>
        "Maxi Savings Account\n"
    }
    val f = { t: Transaction => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs) }
    val transactionSummary = a.transactionSummary(f, "  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(a.sumTransactions())}"
    accountType + transactionSummary + totalSummary
  }

  private def withdrawalOrDepositText(t: Transaction) =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _          => "N/A"
    }

  private def toDollars(number: Double): String = f"$$$number%.2f"
}

