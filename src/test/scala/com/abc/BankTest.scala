package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John").openAccount(new Account(Account.CHECKING))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }
  
  it should "customer summary2" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.CHECKING)
    val savingsAccount: Account = new Account(Account.SAVINGS)
    val john: Customer = new Customer("John").openAccount(checkingAccount)
    val joe: Customer = new Customer("Joe").openAccount(checkingAccount).openAccount(savingsAccount)
    bank.addCustomer(john)
    bank.addCustomer(joe)
    bank.customerSummary should be("Customer Summary\n - John (1 account)\n - Joe (2 accounts)")
  }

  it should "get First Customer Name" in {
    val bank: Bank = new Bank
    bank.getFirstCustomer should be("None")
    val checkingAccount: Account = new Account(Account.CHECKING)
    val john: Customer = new Customer("John").openAccount(checkingAccount)
    bank.addCustomer(john)
    bank.getFirstCustomer should be ("John")
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
    bank.totalInterestPaid should be(3.0)
  }

}
