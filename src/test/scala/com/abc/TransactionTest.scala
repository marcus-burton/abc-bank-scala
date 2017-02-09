package com.abc

/**
  * Created by Ravi on 2/7/2017.
  */


import org.scalatest.{FlatSpec, Matchers}


class TransactionTest extends FlatSpec with Matchers {

  //Note Transaction is a case class with private Constructor
  val tx1 = Transaction( 100.0, TxType.DEPOSIT, getCurrentTime)

  "A new Transaction with 100.0 " should "be an instance of Transaction" in
    {
      tx1.isInstanceOf[Transaction] should be (true)

    }


}
