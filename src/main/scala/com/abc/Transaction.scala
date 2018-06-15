package com.abc

import java.time.LocalDate

case class Transaction(var amount: Double, val transactionDate: LocalDate = DateProvider.now)

