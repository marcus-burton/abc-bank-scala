package com.abc

import java.time.Instant

import com.abc.{Account, Transaction}

class TestTransaction(amount:BigDecimal, date:Instant) extends Transaction(amount:BigDecimal) {
  override val transactionDate = date
}
class TestAccount(accountType:Int) extends Account(accountType){
  def deposit(amount:BigDecimal, date:Instant) = {
    transactions += new TestTransaction(amount, date)
  }
  def withdraw(amount:BigDecimal, date:Instant) = {
    transactions += new TestTransaction(-amount, date)
  }
}
