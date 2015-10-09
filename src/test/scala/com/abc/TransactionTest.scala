package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "set correct amount" in {
    val t = Transaction(5)
    t.amount should be(5)
  }
}
