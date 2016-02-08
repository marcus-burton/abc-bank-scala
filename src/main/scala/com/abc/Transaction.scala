package com.abc

case class Transaction(val amount: Double) {
  val transactionDate = DateProvider.getInstance.now
}

