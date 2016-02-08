package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    val savingsAccount: Account = new Account(Account.SAVINGS)
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
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.openAccount(new Account(Account.CHECKING))
    oscar.numberOfAccounts should be(2)
  }

  it should "testThreeAcounts" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.openAccount(new Account(Account.CHECKING))
    oscar.openAccount(new Account(Account.MAXI_SAVINGS))
    oscar.numberOfAccounts should be(3)
  }

  it should "transfer with sufficient funds" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    val savingsAccount: Account = new Account(Account.SAVINGS)
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    henry.transferFunds(checkingAccount, savingsAccount, 100)
    checkingAccount.sumTransactions should be(0.0)
    savingsAccount.sumTransactions should be(100.0)
  }

  it should "not transfer with insufficient funds" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    val savingsAccount: Account = new Account(Account.SAVINGS)
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    intercept[InsufficientFundsException] {
      henry.transferFunds(checkingAccount, savingsAccount, checkingAccount.sumTransactions+0.01)
    }
  }

  it should "not transfer from not owned account" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    val savingsAccount: Account = new Account(Account.SAVINGS)
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount)
    intercept[TransferFundsException] {
      henry.transferFunds(savingsAccount, checkingAccount, 0)
    }
  }

  it should "not transfer to not owned account" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    val savingsAccount: Account = new Account(Account.SAVINGS)
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount)
    intercept[TransferFundsException] {
      henry.transferFunds(checkingAccount, savingsAccount, 0)
    }
  }

  it should "not deadlock" in {
    val acc1: Account = new Account(Account.CHECKING)
    val acc2: Account = new Account(Account.SAVINGS)
    val henry: Customer = new Customer("Henry").openAccount(acc1).openAccount(acc2)

    acc1.deposit(1000000)
    acc2.deposit(1000000)

    val t1 = thread { for (i <- 0 until 1000) henry.transferFunds(acc1, acc2, 500)}
    val t2 = thread { for (i <- 0 until 1000) henry.transferFunds(acc2, acc1, 500)}
    t1.join(); t2.join();

    acc1.sumTransactions should be eq(acc2.sumTransactions)
  }


  def thread(body: =>Unit): Thread = {
    val t = new Thread {
      override def run() = body
    }
    t.start()
    t
  }
}
