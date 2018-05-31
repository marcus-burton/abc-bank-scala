package com.abc.bank.model

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t = Transaction(5)
    t.isInstanceOf[Transaction] should be(true)
  }
}
