package com.abc

/**
  * Created by Ravi on 2/7/2017.
  */



import org.scalatest.{FlatSpec, Matchers}

class CustomerAccountsTest extends FlatSpec with Matchers {

  // Run incremental tests over the same customer.


  "A new Customer" should "have a 0 Accounts" in {
    val Cust1 = new Customer("Henry")
    assert(Cust1.getNumberOfAccounts == 0)
  }

  "After opening 2 Accounts" should "Cust1 have a 2 Accounts" in {
    val Cust1 = new Customer("Henry")
    Cust1.openAccount(100.00, AccountType.CHECKING)
    Cust1.openAccount(200.00, AccountType.SAVINGS)
    assert(Cust1.getNumberOfAccounts == 2)
  }

  "Trying to open the same type account more than once" should "Throw an Exception" in {
    val Cust1 = new Customer("Henry")
    Cust1.openAccount(100.00, AccountType.SAVINGS)
    assertThrows[RuntimeException] {
      Cust1.openAccount(300, AccountType.SAVINGS)
    }
  }


  "Trying to Transfer from an non-existent source or destination" should "Throw a Run Time Exception" in {
    val Cust1 = new Customer("Henry")
    Cust1.openAccount(100.00, AccountType.SAVINGS)
    Cust1.openAccount(200.00, AccountType.CHECKING)
    assertThrows[RuntimeException] {
      Cust1.transferBetweenAccounts(AccountType.CHECKING, AccountType.MAXI_SAVINGS, 50)
    }
  }

  "Trying to Transfer an account with insufficient funds" should "Throw a Run Time Exception" in {
    val Cust1 = new Customer("Henry")
    Cust1.openAccount(100.00, AccountType.SAVINGS)
    Cust1.openAccount(200.00, AccountType.CHECKING)
    assertThrows[RuntimeException] {
      Cust1.transferBetweenAccounts(AccountType.CHECKING, AccountType.SAVINGS, 500)
    }
  }

  "Trying to Transfer when both Source and Destination Accounts are same" should "Throw a Run Time Exception" in {

    assertThrows[RuntimeException] {
      val Cust1 = new Customer("Henry")
      Cust1.openAccount(100.00, AccountType.SAVINGS)
      Cust1.openAccount(200.00, AccountType.CHECKING)
      Cust1.transferBetweenAccounts(AccountType.CHECKING, AccountType.CHECKING, 50)
    }
  }

}