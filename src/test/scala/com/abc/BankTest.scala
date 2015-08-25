package com.abc

import org.scalatest.{FlatSpec, Matchers}

class BankTest extends FlatSpec with Matchers {

  val bank = new Bank
  val john = new Customer("John")
  val bill = new Customer("Bill")


  "Bank" should "Customers summary" in {
    bank.addCustomer(john)
    john.openAccount(new Account(CHECKING))
    bank.customerSummary should be("Customers Summary\n - John (1 account)")
  }


  it should "checking accounts" in {
    val checkingAccount: Account = new Account(CHECKING)
    bill.openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
    bank.customerSummary should be("Customers Summary\n - John (1 account)\n - Bill (1 account)")

  }

  it should "savings account" in {
    val savingsAccount: Account = new Account(SAVINGS)
    bill.openAccount(savingsAccount)
    savingsAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.1)
    bank.customerSummary should be("Customers Summary\n - John (1 account)\n - Bill (2 accounts)")

  }

  it should "maxi savings account" in {
    val maxiSavingsAccount: Account = new Account(MAXI_SAVINGS)
    bill.openAccount(maxiSavingsAccount)
    maxiSavingsAccount.deposit(3000.0)
    bank.totalInterestPaid should be(172.1)
  }

}
