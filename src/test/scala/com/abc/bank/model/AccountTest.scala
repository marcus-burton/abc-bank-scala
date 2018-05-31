package com.abc.bank.model

import org.scalatest.{FlatSpec, Matchers}

class AccountTest extends FlatSpec with Matchers {

  "Account" should "return deposit transaction" in {
    val acc = new Account(Checking)
    val transaction = acc.deposit(100)
    transaction.isRight shouldBe  true
    transaction.right.get.amount should be(BigDecimal(100))
    acc.sumTransactions() should be(BigDecimal(100))
  }

  it should "return withdraw transaction" in {
    val acc = new Account(Checking)
    val transaction = acc.withdraw(100)
    transaction.isRight shouldBe  true
    transaction.right.get.amount should be(BigDecimal(-100))
    acc.sumTransactions() should be(BigDecimal(-100))
  }

  it should "calculate daily interest" in {
    val acc = new Account(Checking)
    acc.deposit(100)
    acc.addCurrentInterest
    acc.sumTransactions should be(100.03)
  }

}
