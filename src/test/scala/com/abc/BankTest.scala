package com.abc

import org.scalatest.{Matchers, FlatSpec}
import java.util.Date
import java.util.Calendar

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    val john: Customer = new Customer("John").openAccount(new CheckingAccount())
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
    john.openAccount(new SavingsAccount())
    val jane: Customer = new Customer("Jane").openAccount(new MaxiSavingsAccount())
    bank.addCustomer(jane)
    bank.customerSummary should be("Customer Summary\n - John (2 accounts)\n - Jane (1 account)")
  }

  it should "first customer" in {
    val bank: Bank = new Bank
    bank.getFirstCustomer should be("Error")

    val john: Customer = new Customer("John").openAccount(new CheckingAccount())
    bank.addCustomer(john)
    val jane: Customer = new Customer("Jane").openAccount(new MaxiSavingsAccount())
    bank.addCustomer(jane)
    bank.getFirstCustomer should be("John")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new CheckingAccount()
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val savingsAccount: Account = new SavingsAccount()
    bank.addCustomer(new Customer("Bill").openAccount(savingsAccount))
    savingsAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0)
    savingsAccount.withdraw(700)
    bank.totalInterestPaid should be(0.8)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val maxiSavingsAccount: Account = new MaxiSavingsAccount()
    bank.addCustomer(new Customer("Bill").openAccount(maxiSavingsAccount))
    /**
    checkingAccount.deposit(3000.0)
    bank.totalInterestPaid should be(170.0)
    checkingAccount.withdraw(1200.0)
    bank.totalInterestPaid should be(60.0)
    checkingAccount.withdraw(1200.0)
    bank.totalInterestPaid should be(12.0)
      */
    val now:Date  = DateProvider.now
    maxiSavingsAccount.addTransaction(new FakeTransaction(1000, now))
    bank.totalInterestPaid should be(50)

    val cal = Calendar.getInstance()
    cal.setTime(now)
    cal.add(Calendar.DATE, -11)
    val elevenDaysAgo = cal.getTime

    maxiSavingsAccount.addTransaction(new FakeTransaction(-200, elevenDaysAgo))
    bank.totalInterestPaid should be(40)

    cal.add(Calendar.DATE, 3)
    val eightDaysAgo = cal.getTime

    maxiSavingsAccount.addTransaction(new FakeTransaction(-200, eightDaysAgo))

    bank.totalInterestPaid should be(0.6)
  }

  it should "total interest" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new CheckingAccount()
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    val savingsAccount: Account = new SavingsAccount()
    bank.addCustomer(new Customer("Carol").openAccount(savingsAccount))
    savingsAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.1)
  }
}
