package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    var john: Customer = Customer("John").openAccount(Checking())
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = Checking()
    val bill: Customer = Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = Savings()
    bank.addCustomer(Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = MaxiSavings()
    bank.addCustomer(Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(3000.0)
    bank.totalInterestPaid should be(170.0)
  }

  it should "reject negative deposit" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = MaxiSavings()
    bank.addCustomer(Customer("Bill").openAccount(checkingAccount))
    val failMessage = checkingAccount.deposit(-100.0) match {
      case Left(msg) => msg
      case Right(_) => "NOPE"
    }
    failMessage should be("amount must be greater than zero")
  }

  it should "reject negative withdrawal" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = Checking()
    bank.addCustomer(Customer("Bill").openAccount(checkingAccount))
    val failMessage = checkingAccount.withdraw(-100.0) match {
      case Left(msg) => msg
      case Right(_) => "NOPE"
    }
    failMessage should be("amount must be greater than zero")
  }
}
