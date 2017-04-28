package com.abc.transaction

// Not needed at this time but may be later.
object TransactionType extends Enumeration {
  type TransactionType = Value
  val DEPOSIT, WITHDRAWAL, TRANSFER = Value
}
