package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  val customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) {
    customers += customer
  }

  def customerSummary: String = {
    var summary: String = "Customer Summary"
    customers.foreach {c => summary = summary + "\n - " + c.name + " (" + format(c.numberOfAccounts, "account") + ")"}
    summary
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = {
    val total: Double = customers.map(_.totalInterestEarned).reduceLeft(_+_)
    total
  }

  def getFirstCustomer: String = {
    try {
      if(customers.nonEmpty) customers(0).name
      else "Empty"
    }
    catch {
      case e: Exception => {
        e.printStackTrace
        "Error"
      }
    }
  }

}


