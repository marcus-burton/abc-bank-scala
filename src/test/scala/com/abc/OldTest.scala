package com.abc

import com.abc.mutable.{Bank, Customer}
import com.abc.pure.{AccountType, EpochInstantProvider, Transaction}
import org.scalatest.{FlatSpec, Matchers}

// This test driver exists only to demonstrate that the original (deliberately
// subpar) test code still works with only minor modifications.  See the test
// drivers in com.abc.{mutable,pure} for more thorough and idiomatic coverage.

class OldTest extends FlatSpec with Matchers {

  implicit val timestamps = EpochInstantProvider

  "Bank" should "customer summary" in {
    val bank = Bank()
    bank.addCustomer("John").openAccount(AccountType.Checking)
    bank.customerSummary shouldBe "Customer Summary\n - John (1 account)\n"
  }

  it should "checking account" in {
    val bank = Bank()
    bank.addCustomer("Bob").openAccount(AccountType.Checking).deposit(100)
    bank.totalInterestPaid shouldBe BigDecimal("0.1")
  }

  it should "savings account" in {
    val bank = Bank()
    bank.addCustomer("Bob").openAccount(AccountType.Savings).deposit(1500)
    bank.totalInterestPaid shouldBe 2
  }

  it should "maxi savings account" in {
    val bank = Bank()
    bank.addCustomer("Bob").openAccount(AccountType.MaxiSavings).deposit(3000)
    bank.totalInterestPaid shouldBe 170
  }

  "Customer" should "statement" in {
    val henry = Bank().addCustomer("Henry")
    val checking = henry.openAccount(AccountType.Checking)
    val savings = henry.openAccount(AccountType.Savings)
    checking.deposit(100.0)
    savings.deposit(4000.0)
    savings.withdraw(200.0)
    henry.getStatement should be(
      "Statement for Henry\n" +
        "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
        "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
        "\nTotal In All Accounts $3900.00\n")
  }

  it should "testOneAccount" in {
    val oscar = Bank().addCustomer("Oscar")
    oscar.openAccount(AccountType.Savings)
    oscar.numberOfAccounts shouldBe 1
  }

  it should "testTwoAccount" in {
    val oscar = Bank().addCustomer("Oscar")
    oscar.openAccount(AccountType.Savings)
    oscar.openAccount(AccountType.Checking)
    oscar.numberOfAccounts shouldBe 2
  }

  it should "testThreeAcounts" in {
    val oscar = Bank().addCustomer("Oscar")
    oscar.openAccount(AccountType.Savings)
    oscar.openAccount(AccountType.Checking)
    oscar.openAccount(AccountType.MaxiSavings)
    oscar.numberOfAccounts shouldBe 3
  }

  "Transaction" should "type" in {
    val t = new Transaction(5, EpochInstantProvider.now)
    t.isInstanceOf[Transaction] should be(true)
  }
}
