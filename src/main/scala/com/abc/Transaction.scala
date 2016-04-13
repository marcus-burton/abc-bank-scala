package com.abc

import scala.collection.mutable.ListBuffer
import org.joda.time.{ DateTime, LocalDate }

case class Transaction(val amount: BigDecimal, val transactionDate: DateTime)
