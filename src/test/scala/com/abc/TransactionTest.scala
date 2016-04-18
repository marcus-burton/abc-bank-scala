package com.abc

import org.joda.time.{DateTime, Days}
import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t = new Transaction(5)
    t.isInstanceOf[Transaction] should be(true)
  }

  it should "testDateOffset" in {
    val t = new Transaction(500, 5)
    Days.daysBetween(DateTime.now, t.transactionDate).getDays should be(4)
  }
}
