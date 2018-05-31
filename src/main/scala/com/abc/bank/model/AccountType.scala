package com.abc.bank.model

sealed abstract class AccountType
case object Checking extends AccountType {
  override def toString: String = "Checking Account"
}
case object Savings extends AccountType {
  override def toString: String = "Savings Account"
}
case object MaxiSavings extends AccountType {
  override def toString: String = "Maxi Savings Account"
}
