package com.abc

import com.abc.transaction.{DepositTransaction, TransferTransaction, WithdrawalTransaction}
import org.scalatest.{FlatSpec, Matchers}

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account = new Account(1, Account.CHECKING)
    val savingsAccount: Account = new Account(2, Account.SAVINGS)
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)

    DepositTransaction(checkingAccount, 100).process()
    DepositTransaction(savingsAccount, 4000).process()
    WithdrawalTransaction(savingsAccount, 200).process()

    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(1, Account.SAVINGS))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(1, Account.SAVINGS))
    oscar.openAccount(new Account(2, Account.CHECKING))
    oscar.numberOfAccounts should be(2)
  }

  ignore should "testThreeAcounts" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(1, Account.SAVINGS))
    oscar.openAccount(new Account(2, Account.CHECKING))
    oscar.numberOfAccounts should be(3)
  }
}
