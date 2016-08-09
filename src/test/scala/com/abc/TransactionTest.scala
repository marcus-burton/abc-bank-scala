package com.abc

import com.abc.accounts.TransactionType
import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t = new Transaction(5, TransactionType.WITHDRAW)
    t.isInstanceOf[Transaction] should be(true)
  }
}
