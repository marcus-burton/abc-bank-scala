package com.abc

import com.abc.transaction.{DepositTransaction, TransferTransaction, WithdrawalTransaction}
import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {

    val checkingAccount: Account = new Account(1, Account.CHECKING)
    val t = DepositTransaction(checkingAccount, 5)
    t.isInstanceOf[DepositTransaction] should be(true)
  }

  "Deposit Transaction" should "Add money to an account" in {

    val checkingAccount: Account = new Account(1, Account.CHECKING)
    DepositTransaction(checkingAccount, 5).process()

    checkingAccount.balance should be(5)
  }

  "Withdrawal Transaction" should "Remove money from an account" in {

    val checkingAccount: Account = new Account(1, Account.CHECKING)
    WithdrawalTransaction(checkingAccount, 5).process()

    checkingAccount.balance should be(-5)
  }

  "Transfer Transaction" should "Remove money from an account and add to another" in {

    val checkingAccount: Account = new Account(1, Account.CHECKING)
    val savingsAccount: Account = new Account(2, Account.SAVINGS)

    TransferTransaction(checkingAccount, savingsAccount, 10).process()

    checkingAccount.balance should be(-10)
    savingsAccount.balance should be(10)
  }
}
