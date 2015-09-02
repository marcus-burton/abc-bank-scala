package com.abc

import org.scalatest.{FlatSpec, Matchers}

class CustomerTest extends FlatSpec with Matchers {

  "Customer" should "statement" in {
    val checkingAccount = new Account(CHECKING)
    val savingsAccount = new Account(SAVINGS)
    val henry = new Customer("Henry")
    henry.openAccount(checkingAccount)
    henry.openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)

    henry.statement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar = new Customer("Oscar")
    oscar.openAccount(new Account(SAVINGS))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar = new Customer("Oscar")
    oscar.openAccount(new Account(SAVINGS))
    oscar.openAccount(new Account(CHECKING))
    oscar.numberOfAccounts should be(2)
  }

  it should "testThreeAcounts" in {
    val oscar: Customer = new Customer("Oscar")
    oscar.openAccount(new Account(SAVINGS))
    oscar.openAccount(new Account(CHECKING))
    oscar.openAccount(new Account(MAXI_SAVINGS))
    oscar.numberOfAccounts should be(3)
  }

  it should "transferBetweenAccounts" in {
    val oscar: Customer = new Customer("Oscar")
    val checking = new Account(CHECKING)
    val savings = new Account(SAVINGS)
    checking.deposit(500.0)
    savings.deposit(300.0)
    oscar.openAccount(checking)
    oscar.openAccount(savings)
    checking.transferTo(savings, 200.0)
    checking.sumTransactions should be(300.0)
    savings.sumTransactions should be(500.0)
    intercept[UnsupportedOperationException] {
      checking.transferTo(savings, 600.0)
    }
  }
}
