package com.abc

import scala.collection.mutable.ListBuffer

class Bank {
  var customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) {
    customers += customer
  }

  def customerSummary: String = {
    var summary: String = "Customer Summary"
    for (customer <- customers)
      summary = summary + "\n - " + customer.name + " (" + format(customer.numberOfAccounts, "account") + ")"
    summary
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
      customers = null
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
object Bank extends App {

  var bank = new Bank
  var transac = new Transaction(100);
  var lTrasact = ListBuffer(transac)
   var account = new Account(0,lTrasact)
  var laccoount = ListBuffer(account)
  var customer = new Customer("xyz",laccoount )
  bank.addCustomer(customer)
  println(bank.getFirstCustomer)
  println (bank.totalInterestPaid)

}

