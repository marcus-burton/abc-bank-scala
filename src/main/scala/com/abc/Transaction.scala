package com.abc
import java.util.{Calendar, Date}

case class Transaction(val amount: Double) {
  // this is the time at instantiation of the transaction
  val transactionDate: Date = Calendar.getInstance.getTime
}

