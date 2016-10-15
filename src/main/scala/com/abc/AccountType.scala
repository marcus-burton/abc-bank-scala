package com.abc

sealed trait AccountType {

  val label: String

  def interestEarned(amount: Double): Double
}

object Checking extends AccountType {

  override val label = "Checking Account"

  override def interestEarned(amount: Double): Double = {
    val interestRate = 0.001
    amount * interestRate
  }
}

object Savings extends AccountType {

  override val label = "Savings Account"

  override def interestEarned(amount: Double): Double = {
    val interestRateFirst1000 = 0.001
    val interestRateOnRest = 0.002
    if (amount <= 1000) amount * interestRateFirst1000 else 1 + (amount - 1000) * interestRateOnRest
  }
}

object MaxiSavings extends AccountType {

  override val label = "Maxi Savings Account"

  override def interestEarned(amount: Double): Double = {
    val interestRateFirst1000 = 0.02
    val interestRateSecond1000 = 0.05
    val interestRateOnRest = 0.1

    amount match {
      case value if value < 1000 => value * interestRateFirst1000
      case value if value < 2000 => 20 + (value - 1000) * interestRateSecond1000
      case value => 70 + (value - 2000)  * interestRateOnRest
    }
  }
}
