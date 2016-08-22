package com.boloutaredoubeni.bank

import org.scalatest.{FlatSpec, Matchers}

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account = new CheckingAccount
    val savingsAccount: Account = new SavingsAccount
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    henry.statement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new SavingsAccount)
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new SavingsAccount)
    oscar.openAccount(new CheckingAccount)
    oscar.numberOfAccounts should be(2)
  }

  it should "testThreeAccounts" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new SavingsAccount)
    oscar.openAccount(new CheckingAccount)
    oscar.openAccount(new CheckingAccount)
    oscar.numberOfAccounts should be(3)
  }

  it should "transfer from accounts" in {
    val oscar: Customer = new Customer("Oscar")
      .openAccount(new CheckingAccount)
      .openAccount(new SavingsAccount)
    oscar.accounts.head.deposit(30)
    val transactions1 = oscar.accounts.head.sumTransactions()
    val transactions2 = oscar.accounts.last.sumTransactions()
    oscar.transfer(30, oscar.accounts.head, oscar.accounts.last)
    (transactions1 - oscar.accounts.head.sumTransactions()) should be(30)
    (oscar.accounts.last.sumTransactions() - transactions2) should be(30)
  }
}
