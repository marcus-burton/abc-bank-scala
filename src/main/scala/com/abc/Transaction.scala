package com.abc

import com.abc.util.DateProvider
import java.util.Date

object TransactionType extends Enumeration {
  val DEPOSIT = TransactionTypeVal("deposit")
  val WITHDRAWAL = TransactionTypeVal("withdrawal")
  protected case class TransactionTypeVal(name: String) extends super.Val()
  implicit def convert(value: Value) = value.asInstanceOf[TransactionTypeVal]
}

case class Transaction(val amount: Double, val transactionType: TransactionType.Value) {
  val transactionDate = DateProvider.now
  
  //will return a new Date constructed from original Date
  def getTransactionDate: Date = {
		new Date(this.transactionDate.getTime())
	}
}

