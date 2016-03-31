package com.abc

/**
 * A dummy application for a bank
 *
 * The following required features have been implemented:
 * 
 * A customer can open an account
 * A customer can deposit / withdraw funds from an account
 * A customer can request a statement that shows transactions and totals for each of their accounts
 *    Different accounts have interest calculated in different ways
 * Checking accounts have a flat rate of 0.1%
 * Savings accounts have a rate of 0.1% for the first $1,000 then 0.2%
 * Maxi-Savings accounts have a rate of 2% for the first $1,000 then 5% for the next $1,000 then 10%
 * A bank manager can get a report showing the list of customers and how many accounts they have
 * A bank manager can get a report showing the total interest paid by the bank on all accounts

 ### The following additional Features have been implemented:

 * A customer can transfer between their accounts
 * Change Maxi-Savings accounts to have an interest rate of 5% assuming no withdrawals in the past 10 days otherwise 0.1%
 *  
 *  
 * @author feir
 *
 */

import scala.collection.mutable.ListBuffer
import com.abc.util.Utils
import com.typesafe.scalalogging.StrictLogging

class Bank extends StrictLogging {
  logger.info("create abc bank system")
  
  private var customers = new ListBuffer[Customer]

  def addCustomer(customer: Customer) {
    if(Utils.isNull(Option(customer))) {
      val ex: BankException = BankException("can not add null customer to bank", BankExceptionType.BANK_EXCEPTION)
      logger.error("failed to add customer", ex)
      throw ex
    }
    if (customers.contains(customer)){
      val ex: BankException = BankException("customer already exists in the bank", BankExceptionType.BANK_EXCEPTION)
      logger.error("failed to add customer", ex)
      throw ex
    }
    customers += customer
  }

  def customerSummary: String = {
    var summary: String = "Customer Summary"
    for (customer <- customers)
      summary = summary + "\n - " + customer.name + " (" + format(customer.numberOfAccounts, "account") + ")"
    summary
  }

  def totalInterestPaid: Double = {
    var total: Double = 0
    for (c <- customers) total += c.totalInterestEarned
    total
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

}


