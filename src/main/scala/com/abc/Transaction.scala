package com.abc
import java.time.Instant

case class Transaction(var amount: Double, var transactionDate:Instant = DateProvider.getInstance.now)