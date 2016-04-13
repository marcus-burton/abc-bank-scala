package com.abc

import org.joda.time.{ DateTime }

case class Transaction(val amount: BigDecimal, val transactionDate: DateTime)
