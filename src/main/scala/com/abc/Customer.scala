package com.abc

import com.abc.Helpers._

import scala.collection.mutable.ListBuffer

class Customer(val name: String, var accounts: ListBuffer[Account] = ListBuffer()) {

  def openAccount(account: Account) = accounts += account

  def totalInterestEarned = accounts.map(_.interestEarned).sum

  def statement = statementHeader + accounts.map(a => a.statement).mkString("\n", "\n\n", "\n") + statementTotal

  private def statementHeader = s"Statement for $name\n"

  private def statementTotal = s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"

  def totalAcrossAllAccounts = accounts.map(_.sumTransactions).sum

  def summary = s" - $name ($numberOfAccounts ${plurialize(accounts.size, "account")})"

  def numberOfAccounts = accounts.size
}

