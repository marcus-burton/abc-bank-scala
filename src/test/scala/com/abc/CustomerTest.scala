package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account = new Account(Checking)
    val savingsAccount: Account = new Account(Savings)
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Savings))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Savings))
    oscar.openAccount(new Account(Checking))
    oscar.numberOfAccounts should be(2)
  }

  it should "transfer between accounts" in {
    // henry.accounts.indexOf(savingsAccount)
    // henry.accounts(meh).sumTransactions()
    val checkingAccount: Account = new Account(Checking)
    val savingsAccount: Account = new Account(Savings)
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(1000.0)
    savingsAccount.deposit(4000.0)
    checkingAccount.transfer(savingsAccount, 500.0)
    checkingAccount.sumTransactions() should be(500.0)
    savingsAccount.sumTransactions() should be(4500.0)
  }

}