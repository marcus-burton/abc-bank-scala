package com.abc
import org.joda.time.LocalDate
case class Transaction(tranType: Int, var amount: Double, transactionDate :LocalDate= DateProvider.getInstance.now)
  {
  
}

