package com.abc.bank.model

import java.util.Calendar

import org.scalatest.{FlatSpec, Matchers}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    val resultJohn = new Customer("John").openAccount(new Account(Checking))
    resultJohn.isRight shouldBe true
    bank.addCustomer(resultJohn.right.get)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Checking)
    val result = new Customer("Bill").openAccount(checkingAccount)
    result.isRight shouldBe true
    bank.addCustomer(result.right.get)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val savingAccount: Account = new Account(Savings)
    val resultBill = new Customer("Bill").openAccount(savingAccount)
    resultBill.isRight shouldBe true
    bank.addCustomer(resultBill.right.get)
    savingAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account rate 1%" in {
    val bank: Bank = new Bank
    val account: Account = new Account(MaxiSavings)
    bank.addCustomer(new Customer("Bill").openAccount(account).right.get)
    account.deposit(3000.0)
    bank.totalInterestPaid should be(30.0)
  }

  it should "maxi savings account rate 5%" in {
    val bank: Bank = new Bank
    val account: Account = new Account(MaxiSavings)
    bank.addCustomer(new Customer("Bill").openAccount(account).right.get)
    val c = Calendar.getInstance
    c.add(Calendar.DATE, -15)
    val transaction = Transaction(amount = 3000.0, transactionDate = c.getTime)
    account.transactions = List(transaction)
    bank.totalInterestPaid should be(150.0)
  }

}
