package com.abc

import org.scalatest.{Matchers, FlatSpec}
import com.abc.account.CheckingAccount
import com.abc.account.SavingsAccount
import com.abc.account.MaxiSavingsAccount

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John").openAccount(new CheckingAccount())
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = new CheckingAccount()
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    bank.totalInterestPaid should be(0.1)
  }

  it should "savings account" in {
    val bank: Bank = new Bank
    val savingsAccount: Account = new SavingsAccount()
    bank.addCustomer(new Customer("Bill").openAccount(savingsAccount))
    savingsAccount.deposit(1500.0)
    bank.totalInterestPaid should be(2.0)
  }

  it should "maxi savings account" in {
    val bank: Bank = new Bank
    val maxiSavingsAccount: Account = new MaxiSavingsAccount()
    bank.addCustomer(new Customer("Bill").openAccount(maxiSavingsAccount))
    maxiSavingsAccount.deposit(3000.0)
    bank.totalInterestPaid should be(170.0)
  }

  it should "add null customer" in {
    val bank: Bank = new Bank
    
    intercept[BankException] { 
      bank.addCustomer(null)
    }
	}

  it should "add duplicated customer" in {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John")
    
    bank.addCustomer(john)
    intercept[BankException] { 
      bank.addCustomer(john)
    }
  }
    
}
