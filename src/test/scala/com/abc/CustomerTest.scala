package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {
  "Customer statement" should "show transactions and totals for each of their accounts" in {
    val checkingAccount: Account = new Account(Checking)
    val savingsAccount: Account = new Account(Savings)
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    henry.statement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  "Customer" should "be able to open a account" in {
    val oscar: Customer = new Customer("Oscar")
      .openAccount(new Account(Savings))
    oscar.numberOfAccounts should be(1)
  }

  it should "be able to open two accounts" in {
    val oscar: Customer = new Customer("Oscar")
      .openAccount(new Account(Savings))
      .openAccount(new Account(Checking))

    oscar.numberOfAccounts should be(2)
  }

  it should "be able to open three accounts" in {
    val oscar: Customer = new Customer("Oscar")
      .openAccount(new Account(Savings))
      .openAccount(new Account(Checking))
      .openAccount(new Account(MaxiSavings))

    oscar.numberOfAccounts should be(3)
  }

  it should "be able to transfer money between accounts"
}
