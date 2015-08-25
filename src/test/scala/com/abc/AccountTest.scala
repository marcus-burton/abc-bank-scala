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

    val cal1 = Calendar.getInstance()
    cal1.add(Calendar.DAY_OF_MONTH, -60)
    val twoMonthAgo = cal1.getTime


    val cal2 = Calendar.getInstance()
    cal2.add(Calendar.DAY_OF_MONTH, -30)
    val oneMonthAgo = cal2.getTime

    val tt1: Transaction = new TransactionTestImpl(2000, twoMonthAgo)
    msp.transactions += tt1
    msp.interestEarned should be(100.0)

    val tt2: Transaction = new TransactionTestImpl(-1000, oneMonthAgo)
    msp.transactions += tt2
    msp.interestEarned should be(50.0)

    msp.deposit(100)
    msp.interestEarned should be(55.0)

    msp.withdraw(100)
    msp.interestEarned should be(1.0)
  }

}
