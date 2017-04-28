package com.abc

import com.abc.transaction.{DepositTransaction, WithdrawalTransaction}
import org.scalatest.{FlatSpec, Matchers}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John").openAccount(new Account(1, Account.CHECKING))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val account: Account = new Account(1, Account.CHECKING)
    val bill: Customer = new Customer("Bill").openAccount(account)
    bank.addCustomer(bill)
    DepositTransaction(account, 100.0).process()
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val account: Account = new Account(1, Account.SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(account))
    DepositTransaction(account, 1500.0).process()
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val account: Account = new Account(1, Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(account))
    DepositTransaction(account, 3000.0).process()
    bank.totalInterestPaid should be(150.0)
  }

  it should "maxi savings account with a withdrawal" in {
    val bank: Bank = new Bank
    val account: Account = new Account(1, Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(account))
    DepositTransaction(account, 3000.0).process()
    WithdrawalTransaction(account, 500).process()
    bank.totalInterestPaid should be(2.5)
  }

}
