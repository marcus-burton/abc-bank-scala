package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {
  "Customer statement" should "show transactions and totals for each of their accounts" in {

    val henry: Customer = new Customer("Henry")
      .openAccount(accountType = Checking)
      .openAccount(accountType = Savings)

    henry.accounts.foreach {
      case account if account.accountType == Checking => account.deposit(100.0)
      case account if account.accountType == Savings =>
        account.deposit(4000.0)
        account.withdraw(200.0)
    }

    henry.statement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  "Customer" should "be able to open a account" in {
    val oscar: Customer = new Customer("Oscar")
      .openAccount(accountType = Checking)
    oscar.numberOfAccounts should be(1)
  }

  it should "be able to open two accounts" in {
    val oscar: Customer = new Customer("Oscar")
      .openAccount(accountType = Savings)
      .openAccount(accountType = Checking)

    oscar.numberOfAccounts should be(2)
  }

  it should "be able to open three accounts" in {
    val oscar: Customer = new Customer("Oscar")
      .openAccount(accountType = Savings)
      .openAccount(accountType = Checking)
      .openAccount(accountType = MaxiSavings)

    oscar.numberOfAccounts should be(3)
  }

  it should "be able to transfer money between accounts" in {
    val oscar: Customer = new Customer("Oscar")
      .openAccount(accountType = Savings)
      .openAccount(accountType = Checking)
      .openAccount(accountType = MaxiSavings)

    val checkingAccount: Option[oscar.Account] = oscar.accounts.find(_.accountType == Checking)
    val savingAccount: Option[oscar.Account] = oscar.accounts.find(_.accountType == Savings)

    checkingAccount.foreach(_.deposit(1000))

    for {
      account <- checkingAccount
      account1 <- savingAccount
    } {
      oscar.transfer(sourceAccount = account, destinationAccount = account1, 500)
    }

    checkingAccount.map(_.sumTransactions()) should be(Some(500.0))
    savingAccount.map(_.sumTransactions()) should be(Some(500.0))
  }
}
