package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t = new Transaction(5)
    t.isInstanceOf[Transaction] should be(true)
  }

  it should "be a deposit" in {
    val t = Transaction(-5)
    t.transactionType should be("withdrawal")
  }

}
