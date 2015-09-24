package com.abc

import org.scalatest.{ Matchers, FlatSpec }

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John").openAccount(CheckingAccount())
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  val days = if(DateProvider.isLeapYear) 366 else 365

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = CheckingAccount()
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(1000.0)
    checkingAccount.accrueInterst
    bank.totalInterestPaid should be(1.0/days)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = SavingsAccount()
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(1500.0)
    checkingAccount.accrueInterst
    bank.totalInterestPaid should be(2.0/days)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = MaxiSavingsAccount()
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(3000.0)
    checkingAccount.accrueInterst
    bank.totalInterestPaid should be(170.0/days)
  }

}
