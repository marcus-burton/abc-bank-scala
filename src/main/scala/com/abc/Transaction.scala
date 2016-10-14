package com.abc

import java.util.Date

case class Transaction(amount: Double) {
  val transactionDate: Date = DateProvider.instance.now
}

