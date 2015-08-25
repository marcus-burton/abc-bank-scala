package com.abc

sealed trait AccountType {
  def calculateInterest(balance: Double): Double = calculateInterest(balance, None)

  def calculateInterest(balance: Double, recentWithdrawal: Option[Boolean]): Double
}


case object CHECKING extends AccountType {
  override def toString = "Checking Account"

  def calculateInterest(balance: Double, recentWithdrawal: Option[Boolean]) = balance * 0.001
}

case object SAVINGS extends AccountType {
  override def toString = "Savings Account"

  def calculateInterest(balance: Double, recentTrans: Option[Boolean]) = {
    if (balance <= 1000) balance * 0.001
    else 1 + (balance - 1000) * 0.002
  }
}

case object MAXI_SAVINGS extends AccountType {

  override def toString = "Maxi Savings Account"

  def calculateInterest(balance: Double, recentWithdrawal: Option[Boolean]): Double = {
    if (balance <= 1000) balance * 0.02
    if (balance <= 2000) 20 + (balance - 1000) * 0.05
    else 70 + (balance - 2000) * 0.1
  }
}

case object MAXI_SAVINGS_PLUS extends AccountType {

  override def toString = "Maxi Savings Account Plus"

  def calculateInterest(balance: Double, recentWithdrawal: Option[Boolean]): Double = {
    recentWithdrawal match {
      case Some(recentWithdrawalOccurred) => if (recentWithdrawalOccurred) balance * 0.001
      else balance * 0.05
      case None => throw new IllegalArgumentException("this account requires information about recent activity")
    }
  }
}


