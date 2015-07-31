package com.abc

import org.scalatest.{Matchers, FlatSpec}
import java.util.Calendar

class BankTest extends FlatSpec with Matchers {

  "Bank" should "testCustomer summary" in {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John").openAccount(new Account(Account.CHECKING))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "testChecking account interest paid" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.CHECKING)
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "testSavings account interest paid" in {
    val bank: Bank = new Bank
    val savingsAccount: Account = new Account(Account.SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(savingsAccount))
    savingsAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "testMaxi savings account interest paid if no withdrawals in the past 10 days" in {
    val bank: Bank = new Bank
    val maxiSavingsAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(maxiSavingsAccount))
    maxiSavingsAccount.deposit(3000.0)
    bank.totalInterestPaid should be(170.0)
  }
  
  it should "testMaxi savings account interest paid if any withdrawals in the past 10 days" in {
    val bank: Bank = new Bank
    val maxiSavingsAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(maxiSavingsAccount))
    val currDate = Calendar.getInstance
    val tranDate = Calendar.getInstance
    tranDate.set(Calendar.DAY_OF_MONTH, (currDate.get(Calendar.DAY_OF_MONTH) - 20))
    //tranType:1 - deposit
    maxiSavingsAccount.transactions.append(Transaction(1, 3000.0, tranDate.getTime))
    
    tranDate.set(Calendar.DAY_OF_MONTH, (currDate.get(Calendar.DAY_OF_MONTH) - 7))
    //tranType:0 - withdrawal
    maxiSavingsAccount.transactions.append(Transaction(0, -200.0, tranDate.getTime))
    bank.totalInterestPaid should be(101.0)
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
    intercept[IllegalArgumentException] {
      maxiSavingsAccount.withdraw(2500.0)
    }
  }
  
}
