package com.abc

/**
  * dateOffset can be used for simulating date range
  * @param amount
  * @param dateOffset
  */
case class Transaction(var amount: Double, dateOffset: Int = 0) {
  val transactionDate = DateProvider.getInstance.now.plusDays(dateOffset)
}

