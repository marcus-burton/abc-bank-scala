package com.abc

import java.util.Date

trait Transaction {
  def amount: Double
  def transactionDate: Date
}

case class RealTransaction(var amount: Double) extends Transaction {
  def transactionDate = DateProvider.now
}

case class FakeTransaction(var amount: Double, var transactionDate: Date) extends Transaction

