package com.abc

import java.util.ArrayList
import java.util.Collections

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.asScalaIterator

class Bank {
  val customers = Collections.synchronizedList(new ArrayList[Customer]())

  def addCustomer(customer: Customer): Unit = {
    customers += customer
  }

  def customerSummary: String = customers.synchronized {
    "Customer Summary" +
      customers.iterator.map { c =>
        "\n - " + c.name + " (" + format(c.numberOfAccounts, "account") + ")"
      }.mkString
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = customers.synchronized {
    customers.iterator.map { _.totalInterestEarned }.sum
  }

  def accrueInterest(): Unit = customers.synchronized {
    customers.foreach { c =>
      c.accounts.synchronized {
        c.accounts.foreach(a => a.accrueInterst)
      }
    }
  }

  def getFirstCustomer: Option[String] = customers.headOption.map(_.name)
}


