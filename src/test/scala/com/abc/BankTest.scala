package com.abc

import org.scalatest.{Matchers, FlatSpec}
import org.joda.time.DateTime

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John").openAccount(CheckingAccount("c1"))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = CheckingAccount("c1")
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val savingsAccount: Account = SavingsAccount("s1")
    bank.addCustomer(new Customer("Bill").openAccount(savingsAccount))
    savingsAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account with no withdrawals should have 5%" in {
    val bank: Bank = new Bank
    val mxAccount: Account = MaxiSavingsAccount("ms1")
    bank.addCustomer(new Customer("Bill").openAccount(mxAccount))

    mxAccount.deposit(3000.0)
    mxAccount.deposit(1000)
    bank.totalInterestPaid should be(200)
  }

  it should "maxi savings account with withdrawals (in the last 10 days) should have 0.1%" in {
    val bank: Bank = new Bank
    val mxAccount: Account = MaxiSavingsAccount("ms1")
    bank.addCustomer(new Customer("Bill").openAccount(mxAccount))

    mxAccount.deposit(3100.0)
    mxAccount.withdraw(100)
    bank.totalInterestPaid should be(3)
  }

  it should "maxi savings account with withdrawals (before 10 days ago) should have 5%" in {
    val bank: Bank = new Bank
    val mxAccount: Account = MaxiSavingsAccount("ms1")
    bank.addCustomer(new Customer("Bill").openAccount(mxAccount))

    val elevenDaysAgo = DateTime.now.minusDays(11)
    val trans = Transaction(-100, elevenDaysAgo)

    mxAccount.addTransaction(trans)
    mxAccount.deposit(3000.0)
    mxAccount.deposit(1100)
    bank.totalInterestPaid should be(200)
  }

}
