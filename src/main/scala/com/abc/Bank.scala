package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  private val customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) {
    customers += customer
  }

  def customerSummary: String = {
    "Customer Summary" + customers.map(c => "\n - " + c.name + " (" + format(c.numberOfAccounts, "account") + ")")
      .mkString("")
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = {
    customers.foldLeft(0.0)(_ + _.totalInterestEarned)
  }

  def getFirstCustomer: String = {
    customers.headOption.map(_.name).getOrElse("Error")
  }
}


