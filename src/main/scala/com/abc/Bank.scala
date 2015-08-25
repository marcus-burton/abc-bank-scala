package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  var customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) = customers += customer

  def customerSummary = customers.map(c => c.summary).mkString("Customers Summary\n", "\n", "")

  def totalInterestPaid = customers.foldLeft[Double](0)((acc, c) => acc + c.totalInterestEarned)

  def getFirstCustomerName: Option[String] = {
    customers.headOption match {
      case Some(customer) => Some(customer.name)
      case None => None
    }
  }


}


