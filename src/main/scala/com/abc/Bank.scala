package com.abc

import scala.collection.mutable.ArrayBuffer

class Bank {
  val customers = new ArrayBuffer[Customer]

  def addCustomer(customer: Customer) {
    customers += customer
  }

  def customerSummary: String = {
    val summary:String ="Customer Summary"
    val summaryBuilder:StringBuilder=new StringBuilder()
   
     for (customer <- customers){
      summaryBuilder.append(summary)
      summaryBuilder.append("\n - ")
      summaryBuilder.append(customer.name + " (" + format(customer.numberOfAccounts, "account") + ")")
     }
    println("Customer summary="+summary.toString())
    summaryBuilder.toString()
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = {
    var total: Double = 0
    for (c <- customers) total += c.totalInterestEarned
    return total
  }

  def getFirstCustomer: String = {
    try {
      customers(0).name
    }
    catch {
      case e: Exception => {
        e.printStackTrace
        return "Error"
      }
    }
  }

}


