package com.abc.transaction

import java.time.LocalDate


trait Transaction {
  val transactionDate: LocalDate = LocalDate.now()
  val amount: Double

  def process(): Unit
}
