package com.abc

import java.util.Calendar

import org.scalatest.{FlatSpec, Matchers}

class AccountTest extends FlatSpec with Matchers {

  "Account" should "transfer allow transfer between accounts" in {
    val checkingAccount: Account = new Account(Checking)
    val savingsAccount: Account = new Account(Savings)
    checkingAccount.deposit(1000.0)
    savingsAccount.deposit(4000.0)
    checkingAccount.transfer(savingsAccount, 500.0)
    checkingAccount.sumTransactions() should be(500.0)
    savingsAccount.sumTransactions() should be(4500.0)
  }

  "Checking Account" should "have 0.1% interest" in {
    val checkingAccount = new Account(Checking)
    val calendar = Calendar.getInstance()
    calendar.add(Calendar.DAY_OF_MONTH, -365)
    val oneYearAgo = calendar.getTime
    checkingAccount.deposit(1000.0)
    checkingAccount.transactions(0).transactionDate = oneYearAgo
    checkingAccount.interestEarned should be(1)
  }

  "Maxi Savings Plus Account" should "have 5% interest without recent transactions" in {
    val maxiSavingsPlusAccount = new Account(MaxiSavingsPlus)
    val calendar = Calendar.getInstance()
    calendar.add(Calendar.DAY_OF_MONTH, -10)
    val tenDaysAgo = calendar.getTime
    maxiSavingsPlusAccount.deposit(1500)
    maxiSavingsPlusAccount.transactions(0).transactionDate = tenDaysAgo
    val trans: Transaction = Transaction(-500)
    trans.transactionDate = tenDaysAgo
    maxiSavingsPlusAccount.transactions += trans
    maxiSavingsPlusAccount.interestEarned should be(1.34)
  }

  "Maxi Savings Plus Account" should "have 0.1% interest with recent transactions" in {
    val maxiSavingsPlusAccount = new Account(MaxiSavingsPlus)
    val calendar = Calendar.getInstance()
    calendar.add(Calendar.DAY_OF_MONTH, -9)
    val nineDaysAgo = calendar.getTime
    maxiSavingsPlusAccount.deposit(1500)
    maxiSavingsPlusAccount.transactions(0).transactionDate = nineDaysAgo
    val trans: Transaction = Transaction(-500)
    trans.transactionDate = nineDaysAgo
    maxiSavingsPlusAccount.transactions += trans
    maxiSavingsPlusAccount.interestEarned should be(0.02)
  }


}