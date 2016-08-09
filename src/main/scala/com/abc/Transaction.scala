package com.abc

import com.abc.accounts.TransactionType.TransactionType

case class Transaction(var amount: Double, val transactionType: TransactionType) {
  val transactionDate = DateProvider.getInstance.now
}

