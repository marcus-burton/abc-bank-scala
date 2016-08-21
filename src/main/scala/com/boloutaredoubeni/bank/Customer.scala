package com.boloutaredoubeni.bank

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Try}

class Customer(val name: String, var accounts: ListBuffer[Account] = ListBuffer()) {

  def openAccount(account: Account): Customer = {
    accounts += account
    this
  }

  def transfer(amount: Double, from: Account, to: Account) = {
    if (from.id != to.id) {
      for {
        withdrawAccount <- accounts
        if withdrawAccount.id == from.id
      } {
        Try(withdrawAccount.withdraw(amount)) match {
          case Failure(ex) => throw ex
          case _ => ()
        }
      }

      for {
        depositAccount <- accounts
        if depositAccount.id == to.id
      } {
        Try(depositAccount.deposit(amount)) match {
          case Failure(ex) => throw ex
          case _ => ()
        }
      }
    }
  }

  def numberOfAccounts: Int = accounts.size
  def totalInterestEarned: Double = accounts.map(_.interestEarned).sum

  def statement: String = {
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions()).sum
    val accountStatement = f"Statement for $name\n" +
      accounts.map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
    accountStatement
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
}

