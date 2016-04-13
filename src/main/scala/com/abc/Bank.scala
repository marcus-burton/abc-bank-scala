package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  val customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer): Unit = {
    customers += customer
  }

  def customerSummary: String = {
    customers.foldLeft("Customer Summary"){ (summary, customer) =>
      summary + "\n - " + customer.name + " (" + format(customer.numberOfAccounts, "account") + ")"
    }
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: BigDecimal = {
    customers.foldLeft(BigDecimal(0))((total, c) => total + c.totalInterestEarned)
  }

  def getFirstCustomer: String = {
    customers.headOption.fold("Error")(_.name)
  }

}


