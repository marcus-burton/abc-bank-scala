package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t = new RealTransaction(5)
    t.isInstanceOf[Transaction] should be(true)
  }
}
