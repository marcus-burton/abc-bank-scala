package com.abc.bank.model

import com.abc.bank.utils.Util.{CustomerError, Success}

class Bank {

  private var customers = List[Customer]()

  def addCustomer(customer: Customer):Either[CustomerError, Success] =  {
    if (customers.exists(_.name == customer)) {
      Left(s"Can't add customer with name: ${customer.name}. Customer is already exists")
    } else {
      customers.synchronized {
        customers = customer :: customers
      }
      Right()
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


