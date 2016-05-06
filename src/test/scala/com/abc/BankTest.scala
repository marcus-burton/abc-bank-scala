package com.abc

import Account._
/*
object main extends App  {
    val account1 = Savings("John")
    val bank = Bank("bank1")
    bank.addAccount(account1,1).deposit(account1, 100, 2).customerReport
 }
*/
import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {
 
  "Bank" should "customer summary" in {
    
    val account1 = Savings("John")
    val bank = Bank("bank1")
    bank.addAccount(account1,1)
    .deposit(account1, 100, 2)
    .customerReport should be("John, 100.0")
  }
}