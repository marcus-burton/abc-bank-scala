package com.abc
import java.util.Date

/*
case class Transaction(var amount: Double) { 
  val transactionDate = DateProvider.getInstance.now
}
*/
//added tranType and tranDate as part of constructor parameters to implement additional features.
case class Transaction(tranType: Int, amount: Double, tranDate: Date = DateProvider.now)