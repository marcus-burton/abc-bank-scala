package com.abc

import com.abc.accounts.TransactionType

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer


// Refactored to handle all account additions, customer creations, transfers
class Bank {
  var customersAccounts = new HashMap[Customer, ListBuffer[Account]]

  def addCustomer(customer: Customer): String = {
    if(!customerExists(customer))
      customersAccounts += (customer -> new ListBuffer[Account]())
    customer.getId
  }

  def addCustomerWithAccount(customer: Customer, account: Account): String = {
    addCustomer(customer)
    openAccount(customer, account)
    customer.getId
  }

  private def customerExists(customer: Customer): Boolean = customersAccounts.keySet contains customer

  def customerSummary: String = {
    var summary: String = "Customer Summary"
    for (customer <- customersAccounts.keys)
      summary = summary + "\n - " + customer.name + " (" + format(customersAccounts(customer).size, "account") + ")"
    summary
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = {
    var total: Double = 0
    for (c <- customersAccounts.keys) total += calculateInterestForCustomer(c)
    return total
  }

  def openAccount(customer: Customer, account: Account): Customer = {
    customersAccounts(customer) += account
    customer
  }

  def getStatement(customer: Customer): String = {
    //JIRA-123 Change by Joe Bloggs 29/7/1988 start
    var statement: String = null //reset statement to null here
    //JIRA-123 Change by Joe Bloggs 29/7/1988 end
    val accounts = customersAccounts(customer)
    val totalAcrossAllAccounts = accounts.map(_.getBalance).sum
    statement = f"Statement for ${customer.name}\n" +
      accounts.map(_.getStatement).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${Utilities.toDollars(totalAcrossAllAccounts)}"
    statement
  }

  def transfer(customer: Customer, from: Account, to: Account, amount: Double) {
    validateCustomer(customer)
    validateAccount(customer, from)
    validateAccount(customer, to)
    from.transact(amount, TransactionType.WITHDRAW)
    to.transact(amount, TransactionType.DEPOSIT)
  }

  private def validateCustomer(c: Customer) {
    if (!(customersAccounts.keySet contains c)) throw new IllegalArgumentException("No Such Customer")
  }

  private def validateAccount(c: Customer, account: Account) {
    if (!(customersAccounts.keySet contains c) || !(customersAccounts(c) contains account)) throw new IllegalArgumentException("Invalid account requested")
  }

  def numberOfAccounts(customer: Customer) = customersAccounts(customer).size

  private def calculateInterestForCustomer(customer: Customer): Double = {
    customersAccounts(customer).map(_.interestEarned).sum
  }

}


