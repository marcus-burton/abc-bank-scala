package com.abc.bank.model

import com.abc.bank.utils.Util._


class Customer(val name: String) {

  protected var accounts: List[Account] = List.empty[Account]

  //Added either and CustomerError because opening an account is complicated operation (not in this example)
  def openAccount(account: Account): Either[CustomerError, Customer] = {
    accounts.synchronized {
      accounts = accounts :+ account
    }
    Right(this)
  }

  def getAllAccounts = accounts

  def a2aTransfer(from: Account, to: Account, amount: BigDecimal): Either[AccountError, Success] = {
    if (!accounts.contains(from) || !accounts.contains(to))
      Left("Accounts should belong to the same customer")
    else {
      val fromTransaction = from.withdraw(amount)
      fromTransaction match {
        case Left(error) => Left("TransferError: " + error)
        case Right(transaction) =>
          to.deposit(amount) match {
            case Left(error) =>
              from.rollBackTransaction(transaction)
              Left("TransferError: " + error)
            case Right(_) => Right()
          }
      }
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
