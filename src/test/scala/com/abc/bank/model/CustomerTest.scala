package com.abc.bank.model

import org.scalatest.{FlatSpec, Matchers}

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account = new Account(Checking)
    val savingsAccount: Account = new Account(Savings)
    val henry: Customer = new Customer("Henry")
    henry.openAccount(checkingAccount)
    henry.openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  withdrawal $200.00\n  deposit $4000.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar: Customer = new Customer("Oscar")
    oscar.openAccount(new Account(Savings))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = new Customer("Oscar")
    oscar.openAccount(new Account(Savings))
    oscar.openAccount(new Account(Checking))
    oscar.numberOfAccounts should be(2)
  }

  it should "testThreeAccounts" in {
    val oscar: Customer = new Customer("Oscar")
    oscar.openAccount(new Account(Savings))
    oscar.openAccount(new Account(Checking))
    oscar.openAccount(new Account(MaxiSavings))
    oscar.numberOfAccounts should be(3)
  }

  it should "testAccountToAccountTransfer1" in {
    val oscar: Customer = new Customer("Oscar")
    oscar.openAccount(new Account(Savings))
    oscar.openAccount(new Account(MaxiSavings))
    oscar.a2aTransfer(oscar.getAllAccounts(0), oscar.getAllAccounts(1), 100)
    oscar.getAllAccounts(0).sumTransactions() should be(BigDecimal(-100))
    oscar.getAllAccounts(1).sumTransactions() should be(BigDecimal(100))
  }

  it should "testAccountToAccountTransfer different customers" in {
    val oscarAccount = new Account(Savings)
    val tomAccount = new Account(Savings)
    val oscar: Customer = new Customer("Oscar")
    oscar.openAccount(oscarAccount)
    val tom: Customer = new Customer("Tom")
    tom.openAccount(tomAccount)
    val result = oscar.a2aTransfer(oscarAccount, tomAccount, 100)
    result.isLeft shouldBe true
  }
}
