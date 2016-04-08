package com.abc

import org.scalatest.{FlatSpec, Matchers}
import org.joda.time.DateTime

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val now = DateTime.now
    val t = new Transaction(5, now)
    t.isInstanceOf[Transaction] should be(true)
  }
}
