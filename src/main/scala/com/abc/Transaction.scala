package com.abc
import java.util.Date

case class Transaction(var amount: Double) {
  val transactionDate: Date = DateProvider.getInstance.now
}

