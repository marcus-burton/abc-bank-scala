package com.abc

import java.time.Instant

case class Transaction(val amount: Double) {
  val transactionDate = Instant.now
}

