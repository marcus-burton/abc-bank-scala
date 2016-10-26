package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  val customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer): Unit = {
    customers += customer
  }

  def customerSummary: String = {
    customers.foldLeft("Customer Summary"){ (acc: String, customer: Customer) =>
      acc + "\n - " + customer.name + " (" + format(customer.numberOfAccounts, "account") + ")"
    }
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = {
    customers.foldLeft(0.0){ (sum, customer) => sum + customer.totalInterestEarned }
  }

  def getFirstCustomer: String = {
    try {
      customers.head.name
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
        "Error"
    }
  }

}


