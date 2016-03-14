package com.abc.model

class Bank {
  private var customers = List[Customer]()

  def addCustomer(customer: Customer) {
    if (customers.exists(_.name == customer)) {
      throw new Exception(s"Can't add customer with name: ${customer.name}. Customer is already exists")
    } else {
      customers = customer :: customers
    }
  }

  def customerSummary: String = {
    customers.map("Customer Summary\n - " + _.summary).mkString("\n")
  }

  def totalInterestPaid: BigDecimal = {
    customers.foldLeft(BigDecimal(0))(_ + _.totalInterestEarned)
  }

  def getFirstCustomer: Option[String] = customers.headOption.map(_.name)

}


