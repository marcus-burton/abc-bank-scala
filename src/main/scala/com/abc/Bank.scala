package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  var customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer): ListBuffer[Customer] = customers += customer
  def customerSummary: String = customers.map(c=>c.name + " (" + format(c.numberOfAccounts, "account") + ")").mkString("\n - ")

  private[this] def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = customers.map(_.totalInterestEarned).sum
  def getFirstCustomer: Option[String] = customers.headOption.map(_.name)
}


