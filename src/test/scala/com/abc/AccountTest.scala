package com.abc

import java.time.Instant
import org.scalatest.{Matchers, FlatSpec}
import java.time.temporal.ChronoUnit

class AccountTest  extends FlatSpec with Matchers{

  it should "not allow withdraw or deposit 0 amount" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    intercept[InvalidAmountException] {
      checkingAccount.deposit(0)
    }
    intercept[InvalidAmountException] {
      checkingAccount.withdraw(0)
    }
  }


  it should "not allow withdraw or deposit negative amount" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    intercept[InvalidAmountException] {
      checkingAccount.deposit(-1)
    }
    intercept[InvalidAmountException] {
      checkingAccount.withdraw(-1)
    }
  }

  it should "sum amount should net all transactions" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    checkingAccount.deposit(10)
    checkingAccount.withdraw(10)
    checkingAccount.deposit(10)

    checkingAccount.sumTransactions should be(10)
  }


  it should "checking account has flat 0.1% deposited 364days ago" in {
    val account = new TestAccount(Account.CHECKING)
    account.deposit(1000000, Instant.now.minus(364, ChronoUnit.DAYS))
    account.interestEarned should be(BigDecimal(1000000 * 0.1 /100 ).setScale(2,scala.math.BigDecimal.RoundingMode.HALF_EVEN))
  }

  it should "checking account has flat 0.1% deposited today" in {
    val account = new Account(Account.CHECKING)
    account.deposit(1000000)
    account.interestEarned should be(BigDecimal(1000000 * 0.1 /100 /365).setScale(2,scala.math.BigDecimal.RoundingMode.HALF_EVEN))
  }

  it should "savings account has 0.1% for the first 1000" in {
    val account = new TestAccount(Account.SAVINGS)
    account.deposit(1000,Instant.now.minus(364, ChronoUnit.DAYS))
    account.interestEarned should be(1000 * 0.1 /100)
    account.withdraw(100,Instant.now.minus(364, ChronoUnit.DAYS))
    account.interestEarned should be(900 * 0.1 /100)
  }

  it should "savings account has 0.1% for the first 1000 and 0.2% for the rest" in {
    val account = new TestAccount(Account.SAVINGS)
    account.deposit(2000,Instant.now.minus(364, ChronoUnit.DAYS))
    account.interestEarned should be(1000 * 0.1 /100 + 1000 * 0.2 /100)
  }

  it should "maxi savings account has 5% assuming no withdrawals in past 10 days" in {
    val account: Account = new Account(Account.MAXI_SAVINGS)
    account.deposit(100000)
    account.interestEarned should be(BigDecimal(100000 * 5.0 / 100 / 365).setScale(2,scala.math.BigDecimal.RoundingMode.HALF_EVEN))
  }

  it should "maxi savings account has 0.1% if there were withdrawals in past 10 days" in {
    val account = new TestAccount(Account.MAXI_SAVINGS)
    account.deposit(100000, Instant.now.minus(373, ChronoUnit.DAYS))
    account.withdraw(100000, Instant.now.minus(8, ChronoUnit.DAYS))
    account.interestEarned should be(100000 * 0.1 /100)
  }

  it should "check hash" in {
    val account1 = new Account(Account.MAXI_SAVINGS)
    val account2 = new Account(Account.MAXI_SAVINGS)
    account1 should not be eq(account2)
  }

}
