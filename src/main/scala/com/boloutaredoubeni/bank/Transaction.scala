package com.boloutaredoubeni.bank

sealed trait Transaction {
  val _amount: Double
  def amount: Double
  val transactionDate = DateProvider.getInstance.now
}

case class Deposit(_amount: Double) extends Transaction {
  override def amount = _amount
}

case class Withdraw(_amount: Double) extends Transaction {
  override def amount = -_amount
}
