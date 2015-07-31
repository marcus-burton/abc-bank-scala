package com.abc

import org.scalatest.{FlatSpec, Matchers}

class TransactionTest extends FlatSpec with Matchers {
  "Transaction" should "testTransactionInstanceType" in {
    val t = new Transaction(1, 5)
    t.isInstanceOf[Transaction] should be(true)
  }
  
  it should "testTransactionType for deposit" in {
    val tran = Transaction(1, 300.0)
    tran.tranType should be(1)
  }
  
  it should "testTransactionType for withdrawal" in {
    val tran = Transaction(0, 200.0)
    tran.tranType should be(0)
  }   
}
