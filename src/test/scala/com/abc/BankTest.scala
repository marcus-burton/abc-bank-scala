package com.abc

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    val john: Customer = new Customer("John")
      .openAccount(Checking)
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val bill: Customer = new Customer("Bill")
      .openAccount(Checking)
    bank.addCustomer(bill)

    val checkingAccount = bill.accounts
      .collectFirst {
        case account if account.accountType == Checking => account
      }
    checkingAccount.foreach(_.deposit(100.0))

    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val bill: Customer = new Customer("Bill")
      .openAccount(Savings)

    bank.addCustomer(bill)

    val savingAccount = bill.accounts
      .collectFirst {
        case account if account.accountType == Savings => account
      }
    savingAccount.foreach(_.deposit(1500.0))

    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val bill: Customer = new Customer("Bill")
        .openAccount(MaxiSavings)
    bank.addCustomer(bill)

    val maxiSavingsAccount: Option[bill.Account] = bill.accounts.collectFirst {
      case account if account.accountType == MaxiSavings => account
    }
    maxiSavingsAccount.foreach(_.deposit(3000.0))

    bank.totalInterestPaid should be(170.0)
  }

}
