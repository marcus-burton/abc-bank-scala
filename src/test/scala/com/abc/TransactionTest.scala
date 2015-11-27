package com.abc

import org.scalatest.{FlatSpec, Matchers}
import TransactionType._
import java.util.Date

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t = new Transaction(DEPOSIT, 5, new Date())
    t.isInstanceOf[Transaction] should be(true)
  }
}
