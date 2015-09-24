package com.abc

case class Transaction(val amount: Double, val interestAccrural: Boolean = false) {
  val transactionDate = DateProvider.now
}

