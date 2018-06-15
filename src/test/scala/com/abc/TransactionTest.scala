package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val transaction = new Transaction(50.0)
    transaction.isInstanceOf[Transaction] should be(true)
  }
}
