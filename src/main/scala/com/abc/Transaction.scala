package com.abc

import java.time.Instant

case class Transaction(val amount: BigDecimal) {
  val transactionDate = Instant.now
}

