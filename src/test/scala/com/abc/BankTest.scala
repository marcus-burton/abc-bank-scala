package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "create correct customer summary" in {
    val bank: Bank = new Bank
    val john: Customer = new Customer("John").openAccount(Checking())
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "calculate totalInterestPaid for checking account" in {
    val bank: Bank = new Bank
    val bill: Customer = new Customer("Bill")
      .openAccount(Checking().deposit(100.0))
    bank.addCustomer(bill)
    bank.totalInterestPaid should be(0.1)
  }

  it should "calculate totalInterestPaid for savings account" in {
    val bank: Bank = new Bank
    val savingsAccount: Account = Savings()
        .deposit(1500.0)
    bank.addCustomer(new Customer("Bill").openAccount(savingsAccount))
    bank.totalInterestPaid should be(2.0)
  }

  it should "calculate totalInterestPaid for maxi savings account with all deposits" in {
    val bank: Bank = new Bank
    val maxiSavings: Account = MaxiSavings()
      .deposit(3000.0, -20)
      .deposit(3000.0, -15)
      .deposit(3000.0, -10)
      .deposit(3000.0, -5)
    bank.addCustomer(new Customer("Bill").openAccount(maxiSavings))

    val interest =
      3000d * 20 * 0.05 / 365 +
      3000d * 15 * 0.05 / 365 +
      3000d * 10 * 0.05 / 365 +
      3000d * 5 * 0.05 / 365

    Math.abs(bank.totalInterestPaid - interest) should be < 0.000001
  }


  it should "calculate totalInterestPaid for maxi savings account with an old withdrawal" in {
    val bank: Bank = new Bank
    val maxiSavings: Account = MaxiSavings()
      .deposit(3000.0, -20)
      .withdraw(500.0, -15)
      .deposit(3000.0, -10)
      .deposit(3000.0, -5)
    bank.addCustomer(new Customer("Bill").openAccount(maxiSavings))

    val interest =
      3000d * 20 * 0.05 / 365 +
      (-500d * 15 * 0.05 / 365) +
      3000d * 10 * 0.05 / 365 +
      3000d * 5 * 0.05 / 365

    Math.abs(bank.totalInterestPaid - interest) should be < 0.000001
  }


  it should "calculate totalInterestPaid for maxi savings account with an recent withdrawal" in {
    val bank: Bank = new Bank
    val maxiSavings: Account = MaxiSavings()
      .deposit(3000.0, -20)
      .deposit(500.0, -15)
      .deposit(3000.0, -10)
      .withdraw(100.0, -7)
      .deposit(3000.0, -5)
    bank.addCustomer(new Customer("Bill").openAccount(maxiSavings))

    val interest =
      3000d * 20 * 0.001 / 365 +
      500d * 15 * 0.001 / 365 +
      3000d * 10 * 0.001 / 365 +
      -100d * 7 * 0.05 / 365 +
      3000d * 5 * 0.001 / 365

    Math.abs(bank.totalInterestPaid - interest) should be < 0.000001
  }
}
