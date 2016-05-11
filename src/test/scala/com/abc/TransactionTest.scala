package com.abc

import org.scalatest.{FlatSpec, Matchers}
import org.joda.time._

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t = new Transaction(5, DateTime.now, "withdraw")
    t.isInstanceOf[Transaction] should be(true)
  }
}
