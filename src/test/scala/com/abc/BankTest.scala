package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "generate customer summary" in {
    val bank: Bank = new Bank
    val john: Customer = new Customer("John").openAccount(new CheckingAccount())
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "generate interest from checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new CheckingAccount()
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "generate interest from savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new SavingsAccount()
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "generate interest from maxi savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new MaxiSavingsAccount()
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(3000.0)
    bank.totalInterestPaid should be(170.0)
  }

}
