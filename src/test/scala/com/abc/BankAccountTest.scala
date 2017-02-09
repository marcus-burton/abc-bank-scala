package com.abc

/**
  * Created by Ravi on 2/7/2017.
  */

import org.scalatest.{FlatSpec, Matchers}

class BankAccountTest extends FlatSpec with Matchers
{


  "A new Bank" should "have 0 Customers" in
    {
      val bank1 = new Bank
      assert(bank1.custCount == 0)
    }

  "First Customer of new Bank with no Customers" should "be None" in
    {
      val bank1 = new Bank
      assert(bank1.getFirstCustomer == None)

    }

  "After Adding 1 customer" should "Total interest paid by should be as below" in
    {
      val bank1 = new Bank
      val c1 = new  Customer("Henry")
      c1.openAccount(100.00,AccountType.CHECKING)
      bank1.addCustomer(c1)
      assert(bank1.totalInterestPaid == 0.1)
    }

  "After Adding 2 customer" should "Customer Count Should be 2" in
    {
      val bank1 = new Bank
      val c1 = new  Customer("Henry")
      val c2 = new Customer("Heidi")
      c1.openAccount(100.00,AccountType.CHECKING)
      c1.openAccount(200.50,AccountType.SAVINGS)
      c2.openAccount(100.00,AccountType.CHECKING)
      c2.openAccount(200.00,AccountType.SAVINGS)
      bank1.addCustomer(c1)
      bank1.addCustomer(c2)
      assert(bank1.custCount == 2)
    }

  "The total of All balances of all account by all customers" should "sum of all accounts" in
    {

      val bank1 = new Bank
      val c1 = new  Customer("Henry")
      val c2 = new Customer("Heidi")
      c1.openAccount(100.00,AccountType.CHECKING)
      c1.openAccount(200.50,AccountType.SAVINGS)
      c2.openAccount(100.00,AccountType.CHECKING)
      c2.openAccount(200.00,AccountType.SAVINGS)
      bank1.addCustomer(c1)
      bank1.addCustomer(c2)
      assert(bank1.totalAllCustomerAccounts == 600.50 )
    }




  "Total interest Compound Interest Paid to All Customer ( Node default is 30 day and 5% yearly rate" should "be 0.82" in
    {
      val bank1 = new Bank
      val c1 = new  Customer("Henry")
      val c2 = new Customer("Heidi")
      c1.openAccount(100.00,AccountType.CHECKING)
      c1.openAccount(200.50,AccountType.SAVINGS)
      c2.openAccount(100.00,AccountType.CHECKING)
      c2.openAccount(200.00,AccountType.SAVINGS)
      bank1.addCustomer(c1)
      bank1.addCustomer(c2)
      assert(bank1.totalCompoundInterestPaid === 0.82)

    }


}
