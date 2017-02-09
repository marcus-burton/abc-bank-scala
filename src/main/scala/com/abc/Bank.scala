package com.abc

import scala.collection.mutable.ListBuffer


/**
  * Created by Ravi on 2/7/2017.
  */
class Bank {

  private val customers = new ListBuffer[Customer]()

  def addCustomer(c: Customer) = customers += c

  def custCount = customers.size

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  // Return an Option[T]
  def getFirstCustomer: Option[String] = {
    customers.headOption.map(_.name)
  }

  def customerSummary: String = {
    // use StringBuffer or StringBuilder to create summary
    // stringBuilder should be fine as this is single threaded
    val local_string_builder = new StringBuffer()
    local_string_builder.append("--Summary--\n")
    for (cust <- customers) {
      local_string_builder.append(f"${cust.name}%-15s Number of Accounts = ${cust.getNumberOfAccounts}%2d \n")
    }
    local_string_builder.append("--End Summary--\n")
    local_string_builder.toString

  }

  // Sum of all balances held in all the accounts of all customer of the bank at the current moment.
  def totalAllCustomerAccounts = {
    customers.map(_.totalBalanceOverAllAccounts).sum
  }

  def totalInterestPaid = customers.map(_.totalInterestEarned).sum
  def totalCompoundInterestPaid = customers.map(_.totalCompoundInterestEarned).sum

}
