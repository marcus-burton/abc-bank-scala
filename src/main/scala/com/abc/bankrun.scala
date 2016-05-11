package com.abc

import Account._

object main extends App  {
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
  .customerReport(40)
}