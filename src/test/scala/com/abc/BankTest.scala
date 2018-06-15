package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.CHECKING)
    val john: Customer = new Customer("John")
    john.openAccount(checkingAccount)
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.CHECKING)
    val bill: Customer = new Customer("Bill")
    bill.openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val savingsAccount: Account = new Account(Account.SAVINGS)
    val bill: Customer = new Customer("Bill")
    bill.openAccount(savingsAccount)
    bank.addCustomer(bill)
    savingsAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val maxSavingsAccount: Account = new Account(Account.MAXI_SAVINGS)
    val bill: Customer = new Customer("Bill")
    bill.openAccount(maxSavingsAccount)
    bank.addCustomer(bill)
    maxSavingsAccount.deposit(3000.0)
    bank.totalInterestPaid should be(170.0)
  }
  
  it should "first customer" in {
    val bank: Bank = new Bank
    val maxSavingsAccount: Account = new Account(Account.MAXI_SAVINGS)
    val bill: Customer = new Customer("Bill")
    bill.openAccount(maxSavingsAccount)
    bank.addCustomer(bill)
    maxSavingsAccount.deposit(3000.0)
    bank.getFirstCustomer should be("Bill")
  }
  
  it should "transfer" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    val savingsAccount: Account = new Account(Account.SAVINGS)
    val henry: Customer = new Customer("Henry")
    
    henry.openAccount(checkingAccount)
    henry.openAccount(savingsAccount)
    
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.transfer(checkingAccount, 100.0)
    
    savingsAccount.sumTransactions(true) should be (3900.0)
    checkingAccount.sumTransactions(true) should be (200.0)
  }
}
