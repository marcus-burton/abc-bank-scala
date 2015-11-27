package com.abc

import org.scalatest.{Matchers, FlatSpec}
import AccountType._

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account = new Account(CHECKING)
    val savingsAccount: Account = new Account(SAVINGS)
    val henry: Customer = Customer("Henry", checkingAccount, savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    println(henry.getStatement)
    println("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar: Customer = Customer("Oscar", new Account(SAVINGS))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = Customer("Oscar").openAccount(new Account(SAVINGS))
    oscar.openAccount(new Account(CHECKING))
    oscar.numberOfAccounts should be(2)
  }

  ignore should "testThreeAcounts" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(SAVINGS))
    oscar.openAccount(new Account(CHECKING))
    oscar.numberOfAccounts should be(3)
  }

  it should "transfer" in {
    val sa = new Account(SAVINGS)
    val ca = new Account(CHECKING)
    sa.deposit(100)
    ca.deposit(10)
    val oscar: Customer = new Customer("Oscar", sa, ca)
    oscar.transfer(sa, ca, 50)
    sa.sumTransactions() should be(50)
    ca.sumTransactions() should be(60)
  }
}
