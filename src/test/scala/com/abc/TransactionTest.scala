package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t = new Transaction(5)
    t.isInstanceOf[Transaction] should be(true)
  }

  it should "be 0 when incomplete" in {
    val transaction = new Transaction(5, new TransactionState(isComplete = false))
    transaction.amount should be(0)
  }
}
