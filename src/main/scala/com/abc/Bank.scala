package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  val customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) {
    customers += customer
  }

  def customerSummary: String = {
    val summary: String = "Customer Summary\n"
    val customersSummary = customers.map(c => s" - ${c.name} (${format(c.numberOfAccounts, "account")})")
    summary + customersSummary.mkString("\n")
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = {
    customers.map(_.totalInterestEarned).sum
  }

}


