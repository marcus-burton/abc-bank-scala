package com

/**
  * Created by Ravi on 2/7/2017.
  */

import java.text.{DecimalFormat, SimpleDateFormat}
import java.util.Calendar

import scala.math.BigDecimal

package object abc {

  object AccountType extends Enumeration {
    type AccountType = Value
    val CHECKING, SAVINGS, MAXI_SAVINGS = Value
  }

  object TxType extends Enumeration {
    type TxType = Value
    val DEPOSIT, WITHDRAWAL, TRANSFER = Value
  }

  val df = new DecimalFormat("#,##0.00");
  val sdf = new SimpleDateFormat("dd-MM-yyyy hh:mm:ss");

  val cal = Calendar.getInstance()
  def getCurrentTime = sdf.format(cal.getTime)

  //Unique ID to Tx - later
  def uuid = java.util.UUID.randomUUID.toString
  // 128 is the default precision increasing it to 256
  val mc = new java.math.MathContext(256, java.math.RoundingMode.HALF_UP)

  // Some  arbitrary bank wide values for  yearly interest rate 5%
  val default_yearly_iterest_rate = BigDecimal(5.00 / 100)

  def getDailyInterestRate = BigDecimal(default_yearly_iterest_rate.doubleValue() / 365)

  // Since the term is 1 day -- set some default value of 30 days or a month
  def numberOfPeriods = 30

  // note this only calculates interest excluding the principal
  // Note some default values for interest_rate and term have been chosen
  def calcCompoundInterest(interest_rate: BigDecimal = 0.05 / 365, term: Int = 10, principal: BigDecimal) = {
    (principal * ((1 + interest_rate) pow term) - principal).setScale(2, BigDecimal.RoundingMode.HALF_UP)
  }

}
