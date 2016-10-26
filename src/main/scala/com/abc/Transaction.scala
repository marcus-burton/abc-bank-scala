package com.abc
import com.github.nscala_time.time.Imports._

case class Transaction(val amount: Double) {
  // this is the time at instantiation of the transaction
  val date: DateTime = DateTime.now
}

