package com.abc

case class Transaction(var amount: Double) {
  var transactionDate = DateProvider.getInstance.now

  def transactionType: String = amount match {
    case amt if amt < 0 => "withdrawal"
    case amt if amt > 0 => "deposit"
    case _ => "unknown"
  }

}
