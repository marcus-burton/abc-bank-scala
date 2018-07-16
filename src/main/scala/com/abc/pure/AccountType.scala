package com.abc.pure

sealed trait AccountType

object AccountType {
  final case object Checking extends AccountType
  final case object Savings extends AccountType
  final case object MaxiSavings extends AccountType
}
