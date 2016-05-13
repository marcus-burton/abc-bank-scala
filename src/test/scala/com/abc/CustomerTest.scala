package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account = Checking()
      .deposit(100.0)
    val savingsAccount: Account = Savings()
      .deposit(4000.0)
      .withdraw(200.0)
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)

    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(Checking())
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = new Customer("Oscar")
      .openAccount(Savings())
      .openAccount(Checking())
    oscar.numberOfAccounts should be(2)
  }

  it should "testThreeAcounts" in {
    val oscar = new Customer("Oscar")
      .openAccount(Savings())
      .openAccount(Savings())
      .openAccount(Checking())
    oscar.numberOfAccounts should be(3)
  }

  it should "fail transfer when wrong account is passed as 'to' Account" in {
    val oscar = new Customer("Oscar")
      .openAccount(Checking().deposit(100))
      .openAccount(Savings().deposit(200))
    val fromAcct = oscar.accounts.head
    val toAcct = new Savings()
    val ex = intercept[Exception] {
      oscar.transfer(fromAcct, toAcct, 20)
    }
  }

  it should "fail transfer when wrong account is passed as 'from' Account" in {
    val oscar = new Customer("Oscar")
      .openAccount(Checking().deposit(100))
      .openAccount(Savings().deposit(200))
    val fromAcct = Checking()
    val toAcct = oscar.accounts.last
    val ex = intercept[Exception] {
      oscar.transfer(fromAcct, toAcct, 20)
    }
  }

  it should "fail transfer when there is not enough funds in 'from' Account" in {
    val oscar = new Customer("Oscar")
      .openAccount(Checking().deposit(100))
      .openAccount(Savings().deposit(200))
    val fromAcct = oscar.accounts.head
    val toAcct = oscar.accounts.last
    val ex = intercept[Exception] {
      oscar.transfer(fromAcct, toAcct, 200)
    }

    ex.getMessage should include ("Not enough funds to transfer")
  }

  it should "transfer between accounts" in {
    val oscar = new Customer("Oscar")
      .openAccount(Checking().deposit(100d))
      .openAccount(Savings().deposit(200d))
    val fromAcct = oscar.accounts.head
    val toAcct = oscar.accounts.last
    val newOscar = oscar.transfer(fromAcct, toAcct, 20d)

    val newFromAcct = newOscar.accounts.find(_.equals(fromAcct)).getOrElse(Checking())
    val newToAcct = newOscar.accounts.find(_.equals(toAcct)).getOrElse(Savings())

    newFromAcct.sumTransactions() should be(fromAcct.sumTransactions() - 20d)
    newToAcct.sumTransactions() should be(toAcct.sumTransactions() + 20d)
    newOscar.accounts.size should be(oscar.accounts.size)
  }
}
