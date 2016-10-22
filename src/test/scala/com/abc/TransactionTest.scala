package com.abc.test

import com.abc.bank.Bank
import com.abc.bank.account.Account
import com.abc.common.Transaction
import com.abc.bank.account.AccountUtil.{CHECKING, SAVINGS, MAXI_SAVINGS}
import com.abc.bank.customer.Customer
import org.scalatest.{FlatSpec, Matchers}


class TransactionTest extends FlatSpec with Matchers {

  "Transaction" should "type" in {
    val t = new Transaction(5)
    t.isInstanceOf[Transaction] should be(true)
  }




}
