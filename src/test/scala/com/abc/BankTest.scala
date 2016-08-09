package com.abc

import com.abc.accounts._
import org.scalatest.{FlatSpec, Matchers}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    val john: Customer = new Customer("John")
    val checkingAccount = new CheckingAccount
    bank.addCustomerWithAccount(john, checkingAccount)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new CheckingAccount
    val bill: Customer = new Customer("Bill")
    bank.addCustomerWithAccount(bill, checkingAccount)
    checkingAccount.transact(100.0, TransactionType.DEPOSIT)
    bank.totalInterestPaid should be(2.7397260273972606E-4)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new SavingsAccount
    val bill: Customer = new Customer("Bill")
    bank.addCustomerWithAccount(bill, checkingAccount)
    checkingAccount.transact(1500.0, TransactionType.DEPOSIT)
    bank.totalInterestPaid should be(1.0027397260273974)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val maxiSavingsAccount: Account = new MaxiSavingsAccount
    val bill: Customer = new Customer("Bill")
    bank.addCustomerWithAccount(bill, maxiSavingsAccount)
    maxiSavingsAccount.transact(3000.0, TransactionType.DEPOSIT)
    bank.totalInterestPaid should be(0.008219178082191782)
  }

  it should "transfer between accounts" in {
    val bank: Bank = new Bank
    val maxiSavingsAccount: Account = new MaxiSavingsAccount
    val savingsAccount: Account = new SavingsAccount
    val bill: Customer = new Customer("Bill")
    bank.addCustomerWithAccount(bill, maxiSavingsAccount)
    bank.openAccount(bill, savingsAccount)
    savingsAccount.transact(300.0, TransactionType.DEPOSIT)
    bank.transfer(bill, savingsAccount, maxiSavingsAccount, 150.0)
    maxiSavingsAccount.totalBalance should be(150.0)
    savingsAccount.totalBalance should be(150.0)

  }

}
