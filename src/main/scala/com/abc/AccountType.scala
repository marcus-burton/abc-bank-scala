package com.abc

import scala.math.pow

trait AccountType {

  def accruedInterest(interest: Double, days: Int): Double = {
    pow((1 + dailyInterest(interest)), days) - 1
  }

  private def dailyInterest(interest: Double): Double = {
    pow((1 + interest), (1.0 / 365)) - 1
  }

  def interestCalc(amount: Double, days: Int, recentTrans: Option[Boolean]): Double

}

case object Savings extends AccountType {
  def interestCalc(amount: Double, days: Int, recentTrans: Option[Boolean]) = {
    if (amount <= 1000) amount * accruedInterest(Interest.SAVINGS_FIRST_K, days)  //Interest.SAVINGS_FIRST_K
    else (1000 * accruedInterest(Interest.SAVINGS_FIRST_K, days)) + ((amount - 1000) * accruedInterest(Interest.SAVINGS, days)) //1 + (amount - 1000) * Interest.SAVINGS
  }

  override def toString() = "Savings Account"
}

case object MaxiSavings extends AccountType {
  def interestCalc(amount: Double, days: Int, recentTrans: Option[Boolean]): Double = {
    recentTrans match {
      case Some(hadRecentTransaction) if hadRecentTransaction => amount * accruedInterest(0.001, days)
      case _ => amount * accruedInterest(0.05, days)
    }
  }

  override def toString() = "Maxi Savings Account"
}

case object MaxiSavingsPlus extends AccountType {
  def interestCalc(amount: Double, days: Int, recentTrans: Option[Boolean]): Double = {
    recentTrans match {
      case Some(hadRecentTransaction) if hadRecentTransaction => amount * accruedInterest(0.001, days)
      case _ => amount * accruedInterest(0.05, days)
    }
  }

  override def toString = "Maxi Savings Account Plus"
}

case object Checking extends AccountType {
  def interestCalc(amount: Double, days: Int, recentTrans: Option[Boolean]) = amount * accruedInterest(Interest.CHECKING, days)
  //amount * Interest.CHECKING

  override def toString() = "Checking Account"
}