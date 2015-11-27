package com.abc

import scala.collection.mutable.ListBuffer

object Customer {
  def apply(name: String, accounts: Account*) = new Customer(name, accounts: _*)
}

class Customer(val name: String, acts: Account*) extends Format {
  val accounts: ListBuffer[Account] = ListBuffer(acts: _*)
  
  def openAccount(account: Account): Customer = {
    accounts += account
    this
  }
  
  def transfer(from: Account, to: Account, amount: Double) {
    from.withdraw(amount)
    to.deposit(amount)
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double = accounts.map(_.interestEarned).sum

  def getStatement: String = {
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions()).sum
    f"Statement for $name\n" +
      accounts.map(_.statement).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
  }
}

