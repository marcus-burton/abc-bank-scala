package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    val john: Customer = new Customer("John").openAccount(new Account(Account.Checking))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.Checking)
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid() should be(0)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.Savings)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(1500.0)
    bank.totalInterestPaid() should be(0)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.MaxiSavings)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(3000.0)
    bank.totalInterestPaid() should be(0)
  }

}
