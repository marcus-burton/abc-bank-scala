package com.abc

object TransactionType extends Enumeration {
  type TransactionType = Value
  val WITHDRAW, DEPOSIT, INTEREST = Value
}