package com.abc

import java.util.Date

class Transaction(amount_ : Double, transactionState: TransactionState = new TransactionState(isComplete = true)) {
  val transactionDate: Date = DateProvider.instance.now

  def amount: Double = if (transactionState.isComplete) amount_ else 0
}
