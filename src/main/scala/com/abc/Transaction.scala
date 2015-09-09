package com.abc

 class Transaction(var amount: Double) {
  val transactionDate = DateProvider.getInstance.now
}

