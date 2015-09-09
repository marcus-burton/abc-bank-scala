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
    val checkingAccount: Account = new Account(Account.CHECKING)
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val savingsAccount: Account = new Account(Account.SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(savingsAccount))
    savingsAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val maxiSavingsAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(maxiSavingsAccount))
    maxiSavingsAccount.deposit(3000.0)
    bank.totalInterestPaid should be(150.0)
  }
  it should "transfer from checking account to savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.CHECKING)
    val savingsAccount: Account = new Account(Account.SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(savingsAccount).openAccount(checkingAccount))
    checkingAccount.deposit(3000.0)
    savingsAccount.transfer(checkingAccount, 1000)
    bank.totalInterestPaid should be(3.0)
  }  

}
