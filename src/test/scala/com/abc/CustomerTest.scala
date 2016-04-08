package com.abc

import org.scalatest.{Matchers, FlatSpec}
import collection.mutable.{ListBuffer => LB}

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account = CheckingAccount("c1")
    val savingsAccount: Account = SavingsAccount("s1")
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account - c1\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account - s1\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(SavingsAccount("s1"))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(SavingsAccount("s1"))
    oscar.openAccount(CheckingAccount("c1"))
    oscar.numberOfAccounts should be(2)
  }

  it should "testThreeAcounts" in {
    val oscar: Customer = new Customer("Oscar").openAccount(SavingsAccount("s1"))
    oscar.openAccount(CheckingAccount("c1"))
    oscar.openAccount(MaxiSavingsAccount("ms1"))
    oscar.numberOfAccounts should be(3)
  }

  it should "not be allowed to open multiple accounts with the same name" in {
    val checking1 = CheckingAccount("checking")
    checking1.deposit(100)
    val checking2 = CheckingAccount("checking")
    checking2.deposit(50)

    val oscar = new Customer("Oscar")
    noException should be thrownBy oscar.openAccount(checking1)
    an [IllegalArgumentException] should be thrownBy oscar.openAccount(checking2)
  }

  it should "transfer between accounts" in {
    val checking = CheckingAccount("checking")
    val bills = CheckingAccount("billing")
    val savings = SavingsAccount("savings")
    checking.deposit(100)
    bills.deposit(50)
    val oscar = new Customer("Oscar").openAccount(checking)
                                     .openAccount(bills)
                                     .openAccount(savings)
    oscar.transferAmount(50, checking.id, bills.id)
    checking.sumTransactions() should be(50)
    bills.sumTransactions() should be(100)

    oscar.transferAmount(10, checking.id, savings.id)
    savings.sumTransactions() should be(10)
    checking.sumTransactions() should be(40)
  }
}
