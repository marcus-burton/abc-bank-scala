package com.abc

import scala.collection.mutable.ListBuffer

class Customer(val name: String) {
  val accounts: ListBuffer[Account] = ListBuffer()

  def openAccount(account: Account): Customer = {
    accounts += account
    this
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double = accounts.aggregate(0.0)((amt, acct) => amt + acct.interestEarned, _ + _)

  def internalTransfer(a1: Account, a2: Account, amt: Double): Unit = {
    def adj(): Unit = {
      a1.withdraw(amt)
      a2.deposit(amt)
    }
    if (a1.uid < a2.uid) a1.synchronized {
      a2.synchronized {
        adj
      }
    }
    else a2.synchronized {
      a1.synchronized {
        adj
      }
    }
  }

  /**
    * This method gets a statement
    */
  def getStatement: String = {
    println(accounts.map(_.sumTransactions).sum)
    f"Statement for $name\n" +
      accounts.map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(accounts.map(_.sumTransactions).sum)}"
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
    val transactionSummary = a.transactions.map(withdrawalOrDepositText).mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(a.transactions.map(_.amount).sum)}"
    accountType + transactionSummary + totalSummary
  }

  private def withdrawalOrDepositText(t: Transaction) = {
    val amt = t.amount
    if (amt < 0) "withdrawal" + " " + toDollars(t.amount.abs)
    else if (amt > 0) "deposit" + " " + toDollars(t.amount.abs)
    else "N/A" + " " + toDollars(t.amount.abs)
  }

  private def toDollars(number: Double): String = f"$$$number%.2f"
}

