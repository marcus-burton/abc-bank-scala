package com.abc

/**
  * Created by Ravi on 2/5/2017.
  */


import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class AccountTest extends FlatSpec with Matchers
{




  "A new Account" should "have a balance 0" in
    {
      val acc1 = new Account(0.00, AccountType.CHECKING)
      assert(acc1.balance == 0)
    }

  "withdraw from an empty account" should "Throw an Exception" in
    {
      val acc1 = new Account(0.00, AccountType.CHECKING)
      assertThrows[RuntimeException] { acc1.withdraw(10)}
    }


  "After Depositing 10.0 dollar in a Account" should "have a balance of 10" in
    {
      val acc1 = new Account(0.00, AccountType.CHECKING)
      acc1.deposit(10)
      assert(acc1.balance === 10.0)

    }

  "After Depositing 10.0 dollar in a Account with t_deposit" should "Should return Sucess(10)" in
  {
    // Account .deposit should return a Sucess
    val acc1 = new Account(0.00, AccountType.CHECKING)
    acc1.t_deposit(10)
    assert( acc1.t_deposit(10) === Success(10.0))

  }

  "Deposit of a Negative Amount" should "Should Throw an Illegal Argument Exception" in
    {
        val acc1 = new Account(0.00, AccountType.CHECKING)
        assertThrows[IllegalArgumentException] { acc1.deposit(-10)  }
    }

  "Trying to withdraw more amount than available balance with t_withdraw" should "Should  throw an exception" in
    {
      // Account .deposit should return a Sucess
      val acc1 = new Account(0.00, AccountType.CHECKING)
      acc1.t_deposit(10)
      assertThrows[RuntimeException] { acc1.t_withdraw(20)  }
    }



}
