package com.abc

import org.scalatest.{Matchers, FlatSpec}
import org.joda.time._

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    val savingsAccount: Account = new Account(Account.SAVINGS)
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
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.openAccount(new Account(Account.CHECKING))
    oscar.numberOfAccounts should be(2)
  }

  ignore should "testThreeAcounts" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.openAccount(new Account(Account.CHECKING))
    oscar.numberOfAccounts should be(3)
  }

  it should "transfer between accounts with sufficient funds" in{
    val withdrawalAccount: Account = new Account(Account.CHECKING)
    val depositAccount: Account = new Account(Account.CHECKING)
    val benjy: Customer = new Customer("Benjy").openAccount(withdrawalAccount).openAccount(depositAccount)
    withdrawalAccount.deposit(1000.0)
    depositAccount.deposit(217.13)
    benjy.transferBtwnAccounts(137.10, withdrawalAccount, depositAccount) should be("Transfer successful")
  }

  it should "not transfer between accounts with insufficient funds" in{
    val withdrawalAccount: Account = new Account(Account.CHECKING)
    val depositAccount: Account = new Account(Account.CHECKING)
    val benjy: Customer = new Customer("Benjy").openAccount(withdrawalAccount).openAccount(depositAccount)
    withdrawalAccount.deposit(135.10)
    depositAccount.deposit(217.13)
    benjy.transferBtwnAccounts(137.10, withdrawalAccount, depositAccount) should be("Insufficient funds")
  }
}
