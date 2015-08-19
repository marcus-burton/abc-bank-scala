package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John").openAccount(new Account(Account.CHECKING, "Jhon"))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.CHECKING, "Bill")
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.SAVINGS,"Bill")
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val maxsavingAcnt: Account = new Account(Account.MAXI_SAVINGS,"TestUser")
    bank.addCustomer(new Customer("TestUser").openAccount(maxsavingAcnt))
    maxsavingAcnt.deposit(3000.0)
    bank.totalInterestPaid should be(150.0)
  }

  it should "maxi savings account 2" in {
    val bank: Bank = new Bank
    var testUser: Customer = new Customer("TestUser")
    val maxsavingAcnt: Account = new Account(Account.MAXI_SAVINGS,testUser.name)
    bank.addCustomer(testUser.openAccount(maxsavingAcnt))
    maxsavingAcnt.deposit(3000.0)
    maxsavingAcnt.withdraw(1000.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "transfer money from checking to savings" in {
    val bank: Bank = new Bank
    var testUser: Customer = new Customer("TestUser")
    bank.addCustomer(testUser)
    val checkingAccount: Account = new Account(Account.CHECKING,testUser.name)
    val savingsAccount: Account = new Account(Account.SAVINGS,testUser.name)
    testUser.openAccount(checkingAccount)
    testUser.openAccount(savingsAccount)
    checkingAccount.deposit(3000.0)
    savingsAccount.deposit(1000.0)
    var transfer: Transfer = new Transfer(checkingAccount, savingsAccount)
    transfer.makeTransfer(500.0)
    checkingAccount.sumTransactions() should be(2500.0)
    savingsAccount.sumTransactions() should be(1500.0)
  }


}
