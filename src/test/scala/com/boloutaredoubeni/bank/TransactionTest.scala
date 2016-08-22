package com.boloutaredoubeni.bank

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "type" in {
    val t1 = Deposit(5)
    t1.isInstanceOf[Transaction] should be(true)

    val t2 = Withdraw(5)
    t2.isInstanceOf[Transaction] should be(true)
  }
}
