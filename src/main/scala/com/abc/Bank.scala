package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  var customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) { customers += customer }

  def customerSummary: String = customers.foldLeft("Customer Summary")((sum, cus) => sum + BankHelper.formatSummary(cus))

  def totalInterestPaid: Double = customers.aggregate(0.0)((amt, cus)=>amt+cus.totalInterestEarned, (amt1, amt2) =>amt1+amt2)

  def getFirstCustomer: String = {
    val cusOpt = customers.toList.headOption
    cusOpt match {
      case Some(cus) => cus.name
      case None => "Error"
    }
  }

}


