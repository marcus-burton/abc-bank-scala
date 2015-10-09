package com.abc

import scala.collection.JavaConverters._
import java.util.{ArrayList, Collections => JavaCollections}

class Bank {
  val customers = JavaCollections.synchronizedList(new ArrayList[Customer])

  def addCustomer(customer: Customer): Unit =
    customers.add(customer)

  def customerSummary: String = customers.synchronized {
    val customerAccountsText = customers.asScala.map { customer =>
      val accountsText =  format(customer.numberOfAccounts, "account")
      s"\n - ${customer.name} ($accountsText)"
    }.mkString
    s"Customer Summary$customerAccountsText"
  }

  private def format(number: Int, word: String): String = {
    val pluralizedWord = if (number == 1) word else word + "s"
    s"$number $pluralizedWord"
  }

  def totalInterestPaid: Double = customers.synchronized {
    customers.asScala.foldLeft(0.0){ (interestPaid, customer) => 
      interestPaid + customer.totalInterestEarned
    }
  }

  // ok I changed it a bit since I like functions that work on empty lists
  def getFirstCustomer: Option[String] = customers.synchronized {
    customers.asScala.headOption.map(_.name)
  }

}


