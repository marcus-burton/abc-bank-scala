package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  var customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer): Unit = customers += customer

  def totalInterestPaid: Double =  customers.map(_.totalInterestEarned).sum

  private def format(number: Int, word: String): String = s"$number ${if (number == 1) word else word +"s"}"

  def customerSummary: String = {
    val customerSummary =
      customers
        .map(customer => s" - ${customer.name} (${format(customer.numberOfAccounts, "account")})")
        .mkString("\n")

    s"""Customer Summary
       |$customerSummary""".stripMargin
  }

  def firstCustomer: String = {
    try {
      customers = null
      customers.head.name
    }
    catch {
      case e: Exception => {
        e.printStackTrace
        return "Error"
      }
    }
  }
}
