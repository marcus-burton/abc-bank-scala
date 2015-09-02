package com.abc

import java.util.Date

case class TransactionTestImpl(override val amount: Double, override val transactionDate: Date) extends Transaction {
  def transactionType = amount match {
    case a if a < 0 => "withdrawal"
    case a if a > 0 => "deposit"
    case a if a == 0 => "Empty"
  }
}
