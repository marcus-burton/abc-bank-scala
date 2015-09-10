package com.abc

import java.util.Calendar
import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "get customer summary" in {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John").openAccount(new Account(Checking))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "get total interest for checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Checking)
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    val calendar = Calendar.getInstance()
    calendar.add(Calendar.DAY_OF_MONTH, -365)
    val oneYearAgo = calendar.getTime
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    checkingAccount.transactions(0).transactionDate = oneYearAgo
    bank.totalInterestPaid should be(0.1)
  }

  it should "get total interest for savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Savings)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    val calendar = Calendar.getInstance()
    calendar.add(Calendar.DAY_OF_MONTH, -365)
    val oneYearAgo = calendar.getTime
    checkingAccount.deposit(1500.0)
    checkingAccount.transactions(0).transactionDate = oneYearAgo
    bank.totalInterestPaid should be(2.0)
  }

  it should "get total interest for maxi savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(MaxiSavings)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    val calendar = Calendar.getInstance()
    calendar.add(Calendar.DAY_OF_MONTH, -365)
    val oneYearAgo = calendar.getTime
    checkingAccount.deposit(3000.0)
    checkingAccount.transactions(0).transactionDate = oneYearAgo
    checkingAccount.interestEarned should be(150.0)
  }

}
