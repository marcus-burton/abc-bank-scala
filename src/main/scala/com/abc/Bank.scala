package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  var customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) {
    customers += customer
  }

  def customerSummary: String = {
    "Customer Summary" + customers.map(customer => "\n - " + customer.name + " (" + format(customer.numberOfAccounts, "account") + ")").mkString
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = {
    customers.map(_.totalInterestEarned).sum
  }

  def getFirstCustomer: String = {
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


