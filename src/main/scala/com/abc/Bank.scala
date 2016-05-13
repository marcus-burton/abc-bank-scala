package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  val customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) {
    customers += customer
  }

  def customerSummary: String = {
    customers.foldLeft("Customer Summary")((str, cust) => str + "\n - " + cust.printSummary)
  }

  def totalInterestPaid: Double = {
    customers.map(_.totalInterestEarned).sum
  }

  def getFirstCustomer: Option[Customer] = {
    customers.toList match {
      case first :: tail => Some(first)
      case _ => None
    }
  }
}


