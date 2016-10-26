package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {

  "Customer" should "be able to get account statement" in {
    val checkingAccount: Account = new CheckingAccount()
    val savingsAccount: Account = new SavingsAccount()
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "be able to transfer between their accounts" in {
    val checkingAccount: Account = new CheckingAccount()
    val savingsAccount: Account = new SavingsAccount()
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.transfer(checkingAccount, 100)
    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\n  deposit $100.00\nTotal $200.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $100.00\nTotal $3900.00\n" +
      "\nTotal In All Accounts $4100.00")
  }

  it should "be able to open one account" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new SavingsAccount())
    oscar.numberOfAccounts should be(1)
  }

  it should "be able to open two accounts" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new SavingsAccount())
    oscar.openAccount(new CheckingAccount())
    oscar.numberOfAccounts should be(2)
  }

  it should "be able to open three accounts" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new SavingsAccount())
    oscar.openAccount(new CheckingAccount())
    oscar.openAccount(new MaxiSavingsAccount())
    oscar.numberOfAccounts should be(3)
  }
}
