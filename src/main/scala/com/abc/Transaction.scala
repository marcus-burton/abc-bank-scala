package com.abc

case class Transaction(amount: Double) {
  val transactionDate = DateProvider.now
}

