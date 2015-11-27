package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  val customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) {
    customers += customer
  }

  def customerSummary: String = {
    var summary = "Customer Summary"
    for (customer <- customers)
      summary += s"\n - ${customer.name} (${format(customer.numberOfAccounts, "account")})"
    summary
  }

  def format(number: Int, word: String): String = s"$number $word${if(number > 1) "s" else ""}"

  def totalInterestPaid: Double = customers.foldLeft(0.0)((o, p) => o + p.totalInterestEarned)

  def getFirstCustomer: String = {
    if(customers.nonEmpty) customers.head.name else throw new RuntimeException("no customer")
  }

}


