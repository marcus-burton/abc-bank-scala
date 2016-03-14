package com.abc

import com.abc.model.{Account, AccountType, Bank, Customer}
import org.scalatest.{FlatSpec, Matchers}

class AccountTest extends FlatSpec with Matchers {

  "Account" should "return deposit transaction" in {
    val acc = new Account(AccountType.CHECKING)
    val transaction = acc.deposit(100)
    transaction.amount should be(BigDecimal(100))
    acc.sumTransactions() should be(BigDecimal(100))
  }

  it should "return withdraw transaction" in {
    val acc = new Account(AccountType.CHECKING)
    val transaction = acc.withdraw(100)
    transaction.amount should be(BigDecimal(-100))
    acc.sumTransactions() should be(BigDecimal(-100))
  }

  it should "calculate daily interest" in {
    val acc = new Account(AccountType.CHECKING)
    val transaction = acc.deposit(100)

    acc.addCurrentInterest should be(0.03)
  }

}
