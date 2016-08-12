package com.abc

import org.scalatest.{Matchers, FlatSpec}

class AccountTest extends FlatSpec with Matchers {
  it should "have account type value 0 for checking" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    checkingAccount.accountType should be(0)
  }

  it should "have account type value 1 for savings" in {
    val savingsAccount: Account = new Account(Account.SAVINGS)
    savingsAccount.accountType should be(1)
  }

  it should "have account type value 2 for maxisavings" in {
    val maxiSavingsAccount: Account = new Account(Account.MAXI_SAVINGS)
    maxiSavingsAccount.accountType should be(2)
  }

  it should "transfer funds to another account" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    val maxiSavingsAccount: Account = new Account(Account.MAXI_SAVINGS)
    checkingAccount.deposit(1000.00)
    maxiSavingsAccount.deposit(1000.00)
    checkingAccount.transfer(100.00, maxiSavingsAccount)
    maxiSavingsAccount.balance should be(1100.00)
    checkingAccount.balance should be(900.00)
  }

  it should "have balance reflect the sum of transactions" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    checkingAccount.deposit(1000.00)
    checkingAccount.withdraw(100.00)
    checkingAccount.deposit(700.00)
    checkingAccount.balance should be(checkingAccount.sumTransactions())
  }
}
