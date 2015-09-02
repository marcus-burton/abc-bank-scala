package com.abc

import java.util.Calendar

import org.scalatest.{FlatSpec, Matchers}

class AccountTest extends FlatSpec with Matchers {

  val cal1 = Calendar.getInstance()
  cal1.add(Calendar.MONTH, -2)
  val twoMonthAgo = cal1.getTime

  val cal2 = Calendar.getInstance()
  cal2.add(Calendar.MONTH, -1)
  val oneMonthAgo = cal2.getTime

  val cal3 = Calendar.getInstance()
  cal3.add(Calendar.YEAR, -1)
  val oneYearAgo = cal3.getTime

  "MaxiSavingsAccount" should "have 5% interest with recent activity" in {
    val msp = new Account(MAXI_SAVINGS_PLUS)
    msp.deposit(100.0)
    msp.deposit(500.0)
    msp.deposit(400.0)
    msp.interestEarned should be(50)
  }

  "MaxiSavingsAccount" should "have 0.01% interest after recent withdrawal" in {
    val msp = new Account(MAXI_SAVINGS_PLUS)

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

  "CheckingAccount" should "have a lower interest earned on two months with dailyAccrual " in {
    val c = new Account(CHECKING)
    val tt1: Transaction = new TransactionTestImpl(1000, twoMonthAgo)
    c.transactions += tt1
    c.calculateInterestWithDailyAccrual should be(0.16987720850909227)
  }

  it should "have a higher interest earned on one year with dailyAccrual " in {
    val c = new Account(CHECKING)
    val tt1: Transaction = new TransactionTestImpl(1000, oneYearAgo)
    c.transactions += tt1
    c.calculateInterestWithDailyAccrual should be(1.0004987954773696)
  }



}
