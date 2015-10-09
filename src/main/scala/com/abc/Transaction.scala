package com.abc
import java.time.Instant // 

case class Transaction(amount: Double, transactionDate: Instant = DateProvider.now)

