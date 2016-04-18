package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    val john: Customer = new Customer("John").openAccount(new Account(Account.CHECKING))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.CHECKING)
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid - 0.0002739 should be < 0.0000001
  }

  it should "multi Customer test" in {
    val bank: Bank = new Bank
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    val felix: Customer = new Customer("Felix").openAccount(new Account(Account.SAVINGS))
    bank.getFirstCustomer should be("None")
    bank.addCustomer(oscar)
    bank.addCustomer(felix)
    bank.getFirstCustomer should be("Oscar")
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(1500.0)
    bank.totalInterestPaid - 0.002745 should be < 0.000001
  }

  it should "maxi savings account withdrawal last 10" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(3000.0)
    checkingAccount.deposit(30.0, -2)
    checkingAccount.deposit(20.0, -4)
    checkingAccount.deposit(500.0, -6)
    checkingAccount.withdraw(50.0, -9)
    checkingAccount.noWithdrawsLast10 should be(false)
    bank.totalInterestPaid - 0.0958904 should be < 0.0000001
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(3000.0)
    bank.totalInterestPaid - 0.4109589 should be < 0.0000001
  }

}
