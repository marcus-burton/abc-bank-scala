package com.abc

import com.abc.accounts._
import org.scalatest.{FlatSpec, Matchers}


// All these tests can be moved to Bank
class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val bank = new Bank()
    val checkingAccount: Account = new CheckingAccount
    val savingsAccount: Account = new SavingsAccount
    val henry: Customer = new Customer("Henry")
    bank.addCustomerWithAccount(henry, checkingAccount)
    bank.openAccount(henry, savingsAccount)
    checkingAccount.transact(100.0, TransactionType.DEPOSIT)
    savingsAccount.transact(4000.0, TransactionType.DEPOSIT)
    savingsAccount.transact(200.0, TransactionType.WITHDRAW)

    bank.getStatement(henry) should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "multiple transactions on same account" in {
    val bank = new Bank
    val checkingAccount: Account = new CheckingAccount
    val savingsAccount: Account = new SavingsAccount

    val henry: Customer = new Customer("Henry")
    bank.addCustomerWithAccount(henry, checkingAccount)
    bank.openAccount(henry, savingsAccount)

    new Thread(new Runnable() {
      def run() {
        checkingAccount.transact(100.0, TransactionType.DEPOSIT)
      }
    }).start()
    new Thread(new Runnable() {
      def run() {
        savingsAccount.transact(4000.0, TransactionType.DEPOSIT)
      }
    }).start()
    new Thread(new Runnable() {
      def run() {
        savingsAccount.transact(200.0, TransactionType.WITHDRAW)
      }
    }).start()

    new Thread(new Runnable() {
      def run() {
        checkingAccount.transact(500.0, TransactionType.DEPOSIT)
      }
    }).start()

    new Thread(new Runnable() {
      def run() {
        savingsAccount.transact(500.0, TransactionType.WITHDRAW)
      }
    }).start()

    bank.getStatement(henry) should be("Statement for Henry\n\nChecking Account\n  deposit $100.00\n  deposit $500.00\nTotal $600.00\n\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\n  withdrawal $500.00\nTotal $3300.00\n\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val bank = new Bank
    val oscar: Customer = new Customer("Oscar")
    val account = new SavingsAccount
    bank.addCustomerWithAccount(oscar, account)
    bank.numberOfAccounts(oscar) should be(1)
  }

  it should "testTwoAccount" in {
    val bank = new Bank
    val oscar: Customer = new Customer("Oscar")
    val savingsAccount = new SavingsAccount
    val checkingAccount = new CheckingAccount
    bank.addCustomerWithAccount(oscar, savingsAccount)
    bank.openAccount(oscar, checkingAccount)
    bank.numberOfAccounts(oscar) should be(2)
  }

  it should "testThreeAcounts" in {
    val bank = new Bank
    val oscar: Customer = new Customer("Oscar")
    val savingsAccount = new SavingsAccount
    val checkingAccount = new CheckingAccount
    val maxiSavingsAccount = new MaxiSavingsAccount
    bank.addCustomerWithAccount(oscar, savingsAccount)
    bank.openAccount(oscar, checkingAccount)
    bank.openAccount(oscar, maxiSavingsAccount)
    bank.numberOfAccounts(oscar) should be(3)
  }
}
