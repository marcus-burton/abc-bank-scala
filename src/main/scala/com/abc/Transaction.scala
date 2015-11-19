package com.abc

import java.util.Calendar

case class Transaction(method: String, var amount: Double) {
  val transactionDate = Calendar.getInstance()
}