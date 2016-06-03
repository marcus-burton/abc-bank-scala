package com.abc

import java.time.Instant

import scala.collection.mutable.ListBuffer

class Bank {
  val customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) {
    customers += customer
  }

  def customerSummary: String = {
    val parts = customers.map { customer =>
      s"\n - ${customer.name} (${format(customer.numberOfAccounts, "account")})"
    }
    "Customer Summary" + parts.mkString("")
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid(atDate: Instant = Instant.now): BigDecimal = {
    customers.map(_.totalInterestEarned(atDate)).sum
  }

  def firstCustomer: Option[String] = customers.headOption.map(_.name)

}


