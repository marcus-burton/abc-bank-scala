package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John").openAccount(new Account(Account.CHECKING))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val account: Account = new Account(Account.CHECKING)
    val bill: Customer = new Customer("Bill").openAccount(account)
    bank.addCustomer(bill)
    // 365 DAYS, 100.0 => interest = 100.0 * 0.1%
    account.deposit(100.0, DateProvider.spec(2017, 6, 15))
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val account: Account = new Account(Account.SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(account))
    // 365 DAYS, 1500.0 => interest = 1000 * 0.1% + 500 * 0.2% = 2
    account.deposit(1500.0, DateProvider.spec(2017,6,15))
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val account: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(account))
    // 365 DAYS, 3000.0, not withdraw in 10 days => interest = 3000 * 5% = 150
    account.deposit(3000.0, DateProvider.spec(2017,6,15))
    bank.totalInterestPaid should be(150.0)
  }

  it should "maxi savings account 2" in {
    val bank: Bank = new Bank
    val account: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(account))
    // 365 DAYS, 3000.0, not withdraw in 10 days => interest = 3000 * 5% = 150
    account.deposit(3000.0, DateProvider.spec(2017,6,15))
    // 5 DAYS before, withdraw 1000.0, total 2000.0 => interest = 2000 * 0.1% * 5/365 = 0.03
    account.withdraw(1000.0, DateProvider.spec(2018,6,10))
    bank.totalInterestPaid should be(0.03)
  }
}
