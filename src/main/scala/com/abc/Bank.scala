package com.abc

object Bank {

  def addCustomer(customer: Customer, customers: Customers): Customers = customers :+ customer

  def totalInterestPaid(customers: Customers, accounts: Accounts, transactions: Transactions): Double =
    customers.map(customer => customer.totalInterestEarned(customer.getAccounts(accounts), transactions)).sum

  private def format(number: Int, word: String): String = s"$number ${if (number == 1) word else word +"s"}"

  def customerSummary(customers: Customers, accounts: Accounts): String = {
    val customerSummary =
      customers  map { customer =>
        val customerAccounts = customer.getAccounts(accounts)
        s" - ${customer.name} (${format(customer.numberOfAccounts(customerAccounts), "account")})"
      } mkString "\n"

    s"""Customer Summary
       |$customerSummary""".stripMargin
  }

  def firstCustomer(customers: Customers): Option[String] = customers.headOption.map(_.name)
}
