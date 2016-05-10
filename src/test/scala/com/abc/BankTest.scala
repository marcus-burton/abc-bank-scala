package com.abc

import Account._

import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {
 
  "Bank" should "customer summary" in {

    val account1 = Savings("John")
    val bank = Bank("bank1")
    bank.addAccount(account1,5)
      .deposit(account1, 100, 11)
      .deposit(account1, 1000, 15)
      .deposit(account1, 2000, 18)
      .deposit(account1, 5000, 20)
      .customerReport(account1,40) should be("John, 8424.871146928395")

    val account2 = Checking("Jim")
    bank.addAccount(account2,5)
      .deposit(account2, 100, 11)
      .deposit(account2, 1000, 15)
      .deposit(account2, 2000, 18)
      .deposit(account2, 5000, 20)
      .customerReport(account2,40) should be("Jim, 8273.66409947586")

    val account3 = MaxSavings("Jack")
    bank.addAccount(account3,5)
      .deposit(account3, 100, 11)
      .deposit(account3, 1000, 15)
      .deposit(account3, 2000, 18)
      .deposit(account3, 5000, 20)
      .customerReport(account3,40) should be("Jack, 54057.77422840032")
  }
}
