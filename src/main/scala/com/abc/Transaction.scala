package com.abc

import java.time.Instant

case class Transaction(amount: BigDecimal, transactionDate: Instant = DateProvider.now)