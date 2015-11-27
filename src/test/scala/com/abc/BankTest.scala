package com.abc

import org.scalatest.{Matchers, FlatSpec}
import AccountType._

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    var john: Customer = Customer("John", new Account(CHECKING))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(CHECKING)
    val bill: Customer = Customer("Bill", checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(SAVINGS)
    bank.addCustomer(Customer("Bill", checkingAccount))
    checkingAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(MAXI_SAVINGS)
    bank.addCustomer(Customer("Bill", checkingAccount))
    checkingAccount.deposit(3000.0)
    bank.totalInterestPaid should be(170.0)
  }

}
