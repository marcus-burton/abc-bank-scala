package com.abc

import org.scalatest.{Matchers, FlatSpec}
import java.util.Calendar
import org.joda.time.LocalDate
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
    bank.totalInterestPaid should be(0.10005001667083846)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val savingsAccount: Account = new Account(Account.SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(savingsAccount))
    savingsAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0010006670001514)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val maxiSavingsAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(maxiSavingsAccount))
    maxiSavingsAccount.deposit(3000.0)
    bank.totalInterestPaid should be(153.81328912807234)
  }
  it should "transfer from checking account to savings account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.CHECKING)
    val savingsAccount: Account = new Account(Account.SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(savingsAccount).openAccount(checkingAccount))
    checkingAccount.deposit(3000.0)
    savingsAccount.transfer(checkingAccount, 1000)
    bank.totalInterestPaid should be(3.001500500125154)
  }  
it should "testMaxi savings account interest paid if no withdrawals in the past 10 days" in {
    val bank: Bank = new Bank
    val maxiSavingsAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(maxiSavingsAccount))
    maxiSavingsAccount.deposit(3000.0)
    bank.totalInterestPaid should be(153.81328912807234)
  }
  
  it should "testMaxi savings account interest paid if any withdrawals in the past 10 days" in {
    val bank: Bank = new Bank
    val maxiSavingsAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(maxiSavingsAccount))
    val currDate = Calendar.getInstance
    val tranDate = Calendar.getInstance
    tranDate.set(Calendar.DAY_OF_MONTH, (currDate.get(Calendar.DAY_OF_MONTH) - 20))
    //tranType:1 - deposit
    maxiSavingsAccount.deposit(3000.0)
    tranDate.set(Calendar.DAY_OF_MONTH, (currDate.get(Calendar.DAY_OF_MONTH) - 7))
    //tranType:0 - withdrawal
    maxiSavingsAccount.withdraw(-200.0)
    bank.totalInterestPaid should be(164.06750840327717)
  }
  
  //added test cases to validate deposit and withdrawel transactions
  
  it should "test account deposit if deposit amount <= 0" in {
    val bank: Bank = new Bank
    val maxiSavingsAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(maxiSavingsAccount))    
    intercept[IllegalArgumentException] {
      maxiSavingsAccount.deposit(-100.0)
    }
  }
  
  it should "test account withdrawal if withdrawal amount <= 0" in {
    val bank: Bank = new Bank
    val maxiSavingsAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(maxiSavingsAccount))    
    intercept[IllegalArgumentException] {
      maxiSavingsAccount.deposit(-200.0)
    }
  }
  
  it should "test account withdrawal if withdrawal amount > available balance" in {
    val bank: Bank = new Bank
    val maxiSavingsAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(maxiSavingsAccount)) 
    maxiSavingsAccount.deposit(2000.0)
    intercept[InsufficientFundsException] {
      maxiSavingsAccount.withdraw(2500.0)
    }
  }
}
