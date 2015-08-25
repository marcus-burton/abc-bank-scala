package com.abc

trait Transaction {
  val amount: Double
  val transactionDate = DateProvider.getInstance.now

  def transactionType: String
}

case class TransactionImpl(override val amount: Double) extends Transaction {
  def transactionType = amount match {
    case a if a < 0 => "withdrawal"
    case a if a > 0 => "deposit"
  }
}


