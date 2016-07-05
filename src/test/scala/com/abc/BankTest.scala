package com.abc

import org.scalatest.{Matchers, FlatSpec}
import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.temporal.ChronoUnit

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
    val checkingAccount: Account = new Account(Account.SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    checkingAccount.deposit(3000.0)
    bank.totalInterestPaid should be(170.0)
  }
it should "maxi savings account accrued daily over ten days" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    val time = LocalDateTime.now()
    checkingAccount.deposit(3000.0, time.minus(15, ChronoUnit.DAYS).toInstant(ZoneOffset.UTC))
    bank.totalInterestPaidAccruedDaily should be(6.16)
  }
it should "maxi savings account accrued daily over ten days withdrawn amount" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    val time = LocalDateTime.now()
    checkingAccount.deposit(3000.0, time.minus(15, ChronoUnit.DAYS).toInstant(ZoneOffset.UTC))
    checkingAccount.withdraw(1000.0, time.minus(9, ChronoUnit.DAYS).toInstant(ZoneOffset.UTC))
    bank.totalInterestPaidAccruedDaily should be(6.49)
  }
 it should "maxi savings account accrued daily less than ten days" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
    val time = LocalDateTime.now()
    checkingAccount.deposit(3000.0, time.minus(9, ChronoUnit.DAYS).toInstant(ZoneOffset.UTC))
    bank.totalInterestPaidAccruedDaily should be(3.7)
  }
}
