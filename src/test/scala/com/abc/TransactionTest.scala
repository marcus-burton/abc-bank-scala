package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t = new Transaction(5)
    t.isInstanceOf[Transaction] should be(true)
  }

  "Transaction" should "store lots of money" in {
    val t = new Transaction(new Transaction(Double.MaxValue).amount + new Transaction(Double.MaxValue).amount)
    t.amount should be (2 * BigDecimal(Double.MaxValue))
  }
}
