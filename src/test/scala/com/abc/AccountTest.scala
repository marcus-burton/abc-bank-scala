package com.abc

import org.scalatest.{FlatSpec, Matchers}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


class AccountTest extends FlatSpec with Matchers {

  it should "deposit" in {
    val account: Account = new Account(Account.SAVINGS)
    account.deposit(10)
    account.deposit(20)
    account.balance should be(30)
    account.transactions(0).amount should be (10.0)
  }

  it should "deposit negative amt and expect exception" in {
    val account: Account = new Account(Account.SAVINGS)
    val thrown = intercept[Exception] {
      account.deposit(-10)
    }
    assert(thrown.getMessage === "amount must be greater than zero")
  }

  it should "withdraw" in {
    val account: Account = new Account(Account.SAVINGS)
    account.deposit(100)
    account.withdraw(50)
    account.balance should be(50)
    account.transactions(1).amount should be (-50.0)
  }

  it should "withdraw negative amt and expect exception" in {
    val account: Account = new Account(Account.SAVINGS)
    val thrown = intercept[Exception] {
      account.withdraw(-10)
    }
    assert(thrown.getMessage === "amount must be greater than zero")
  }

  it should "mix depost and withdraw" in {
    val account: Account = new Account(Account.SAVINGS)
    account.deposit(100)
    account.withdraw(50)
    account.deposit(30)
    account.deposit(20)
    account.deposit(60)
    account.withdraw(60)
    account.withdraw(10)
    account.balance should be(90)
    account.transactions.size should be (7)
  }

  it should "mix depost and withdraw at the same time" in {
    val account: Account = new Account(Account.SAVINGS)
    val act1 = Future { account.deposit(100)
                        account.withdraw(50)
                        account.deposit(30)
                        account.deposit(20) }
    val act2 = Future { account.deposit(60)
                        account.withdraw(60)
                        account.withdraw(10) }
    act1 onSuccess {
      case _ =>  {
        act2 onSuccess { case _ => {
          account.balance should be(90)
          account.transactions.size should be (7)}
        }
      }
    }

  }

  it should "has withdraw last 10 days" in {
    val account: Account = new Account(Account.SAVINGS)
    account.deposit(100)
    account.withdraw(50)
    account.hasWithdrawWithinDays(10) should be (true)
  }

  it should "has no withdraw last 10 days" in {
    val account: Account = new Account(Account.SAVINGS)
    account.deposit(100)
    account.hasWithdrawWithinDays(10) should be (false)
  }

  it should "savings interest earned balance less than 1000" in {
    val account: Account = new Account(Account.SAVINGS)
    account.deposit(100)
    account.interestEarned should be (0.1)
  }

  it should "savings interest earned balance more than 1000" in {
    val account: Account = new Account(Account.SAVINGS)
    account.deposit(2000)
    account.interestEarned should be (3.0)
  }

  it should "max savings interest earned balance no withdraw" in {
    val account: Account = new Account(Account.MAXI_SAVINGS)
    account.deposit(2000)
    account.interestEarned should be (100.0)
  }

  it should "max savings interest earned balance with withdraw" in {
    val account: Account = new Account(Account.MAXI_SAVINGS)
    account.deposit(2000)
    account.withdraw(1000)
    account.interestEarned should be (1.0)
  }

  it should "max savings interest earned balance with checking" in {
    val account: Account = new Account(Account.CHECKING)
    account.deposit(2000)
    account.withdraw(1000)
    account.interestEarned should be (1.0)
  }







}
