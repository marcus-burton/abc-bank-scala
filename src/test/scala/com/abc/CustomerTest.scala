package com.abc

import org.scalatest.{ Matchers, FlatSpec }
import scala.util.Failure

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account = CheckingAccount()
    val savingsAccount: Account = SavingsAccount()
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(SavingsAccount())
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(SavingsAccount())
    oscar.openAccount(CheckingAccount())
    oscar.numberOfAccounts should be(2)
  }

  it should "transfer successfully" in {
    val checkingAccount = CheckingAccount()
    val savingsAccount = SavingsAccount()
    val oscar: Customer = new Customer("Oscar").openAccount(savingsAccount)
    oscar.openAccount(checkingAccount)
    checkingAccount.deposit(1000)
    savingsAccount.deposit(1000)
    oscar.transfer(checkingAccount, savingsAccount, 500)
    checkingAccount.sumTransactions() should be(500)
    savingsAccount.sumTransactions() should be(1500)
  }

  it should "fail a transfer" in {
    val checkingAccount = CheckingAccount()
    val savingsAccount = SavingsAccount()
    val oscar: Customer = new Customer("Oscar").openAccount(savingsAccount)
    oscar.openAccount(checkingAccount)
    checkingAccount.deposit(1000)
    savingsAccount.deposit(1000)
    intercept[IllegalArgumentException] {
      oscar.transfer(checkingAccount, savingsAccount, -1500).get
    }
  }

  ignore should "testThreeAcounts" in {
    val oscar: Customer = new Customer("Oscar").openAccount(SavingsAccount())
    oscar.openAccount(CheckingAccount())
    oscar.numberOfAccounts should be(3)
  }
}
