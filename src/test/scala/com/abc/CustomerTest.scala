package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account = new Account(Account.Checking)
    val savingsAccount: Account = new Account(Account.Savings)
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
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.Savings))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.Savings))
    oscar.openAccount(new Account(Account.Checking))
    oscar.numberOfAccounts should be(2)
  }

  it should "testThreeAcounts" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.Savings))
    oscar.openAccount(new Account(Account.Checking))
    oscar.openAccount(new Account(Account.MaxiSavings))
    oscar.numberOfAccounts should be(3)
  }

  it should "be able to transfer money between acounts" in {
    val savings = new Account(Account.Savings)
    val checking = new Account(Account.Checking)
    val oscar = new Customer("Oscar").openAccount(savings)
    oscar.openAccount(checking)

    checking.deposit(100.0)
    savings.deposit(15.0)

    checking.transferTo(savings, 50.0)

    checking.sumTransactions() should be (50.0)
    savings.sumTransactions() should be (65.0)
  }
}
