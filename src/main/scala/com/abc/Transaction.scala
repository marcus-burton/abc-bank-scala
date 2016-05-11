package com.abc

import org.joda.time._

case class Transaction(val amount: Double, val time: DateTime, val transType: String)

//added a time and type of transaction to more easily account for all transactions in past 10 days.

