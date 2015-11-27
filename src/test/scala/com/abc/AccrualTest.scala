package com.abc

import org.scalatest.{Matchers, FlatSpec}
import AccountType._
import java.text.SimpleDateFormat

class AccrualTest extends FlatSpec with Matchers {
  val fmt = new SimpleDateFormat("yyyy-MM-dd")
  
  it should "no withdraw rate at 5%" in {
    val ma = new Account(MAXI_SAVINGS)
    ma.deposit(10000, fmt.parse("2015-11-01"))
    ma.accrual(fmt.parse("2015-11-12"))
    println(ma.sumTransactions())
    ma.sumTransactions() should be (10001.3689)
  }

  it should "no recent withdraw rate at 5%" in {
    val ma = new Account(MAXI_SAVINGS)
    ma.deposit(20000, fmt.parse("2015-11-01"))
    ma.withdraw(10000, fmt.parse("2015-11-01"))
    ma.accrual(fmt.parse("2015-11-12"))
    println(ma.sumTransactions())
    ma.sumTransactions() should be (10001.3689)
  }  

  it should "recent withdraw rate at 0.1%" in {
    val ma = new Account(MAXI_SAVINGS)
    ma.deposit(20000, fmt.parse("2015-11-01"))
    ma.withdraw(10000, fmt.parse("2015-11-02"))
    ma.accrual(fmt.parse("2015-11-12"))
    println(ma.sumTransactions())
    ma.sumTransactions() should be (10000.0274)
  }  
}
