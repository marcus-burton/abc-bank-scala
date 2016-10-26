package com.abc
import java.util.Date

case class Transaction(val amount: Double) {
  val transactionDate: Date = DateProvider.now
}

