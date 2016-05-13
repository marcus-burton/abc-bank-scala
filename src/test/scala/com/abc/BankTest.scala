package com.abc

import Account._

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {
 
  "Bank" should "customer summary" in {

    val account1 = Savings("John")
    val account11 = Checking("John")
    val account2 = Checking("Jim")
    val account3 = MaxSavings("Jack")

    val bank = Bank("bank1")
      .addAccount(account1,5)
      .deposit(account1, 100, 11)
      .deposit(account1, 1000, 15)
      .addAccount(account11,16)
      .deposit(account1, 2000, 18)
      .doTransfer(account1,account11,200,19)
      .deposit(account1, 5000, 21)
      .addAccount(account2,5)
      .deposit(account2, 100, 11)
      .deposit(account2, 1000, 15)
      .deposit(account2, 2000, 18)
      .deposit(account2, 5000, 20)
      .addAccount(account3,5)
      .deposit(account3, 100, 11)
      .deposit(account3, 1000, 15)
      .deposit(account3, 2000, 18)
      .deposit(account3, 5000, 20)
      .customerReport(40) should be("\nJim has checking account  with current balance = 8104.710904795356, including accrued interest = 4.710904795356014\n" +
      "\nJack has maxi account  with current balance = 8141.641856144109, including accrued interest = 41.64185614410875\n" +
      "\nJohn has checking account  with current balance = 200.1151000242361, including accrued interest = 0.11510002423611919\n" +
      "John has savings account  with current balance = 7908.355235675221, including accrued interest = 8.35523567522043\n")
  }
}
