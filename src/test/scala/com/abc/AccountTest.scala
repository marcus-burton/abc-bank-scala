package com.abc

import java.util.Calendar

import org.scalatest.{FlatSpec, Matchers}

class AccountTest extends FlatSpec with Matchers {

  "MaxiSavingsAccount" should "have 5% interest with recent activity" in {
    val msp = new Account(MAXI_SAVINGS_PLUS)
    msp.deposit(100.0)
    msp.deposit(500.0)
    msp.deposit(400.0)
    msp.interestEarned should be(50)
  }

  "MaxiSavingsAccount" should "have 0.01% interest with recent transactions" in {
    val msp = new Account(MAXI_SAVINGS_PLUS)
    val calendar = Calendar.getInstance()
    calendar.add(Calendar.DAY_OF_MONTH, -30)
    val oneMonthAgo = calendar.getTime
    val tt: Transaction = new TransactionTestImpl(1000, oneMonthAgo)
    msp.transactions += tt
    msp.interestEarned should be(1.0)
    msp.deposit(1000)
    msp.interestEarned should be(100.0)
  }

}
