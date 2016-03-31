package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t = new Transaction(5, TransactionType.DEPOSIT)
    t.isInstanceOf[Transaction] should be(true)
  }
  
}
