package com.abc.model

import com.abc.utils.Util._

import scala.util.{Success, Failure, Try}

class Customer(val name: String) {

  private var accounts: List[Account] = List.empty

  def openAccount(account: Account): Customer = {
    accounts = accounts :+ account
    this
  }

  def getAllAccounts = accounts

  def a2aTransfer(from: Account, to: Account, amount: BigDecimal) {
    if (!accounts.contains(from) || accounts.contains(to))
      throw new Exception("Accounts should belong to the same customer")
    val fromTransaction = from.withdraw(amount)
    Try{ to.deposit(amount)} match {
      case Failure(ex) => from.rollBackTransaction(fromTransaction)
        throw ex
      case Success(v) =>
    }
  }

  def totalInterestEarned: BigDecimal = accounts.map(_.interestEarned).sum


  def getStatement: String = {
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions()).sum
    f"Statement for $name\n" +
      accounts.map(_.statement).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${totalAcrossAllAccounts.toDollars}"
  }

  def summary = name + " (" + format(numberOfAccounts, "account") + ")"

  def numberOfAccounts: Int = accounts.size

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }


}

