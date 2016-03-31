package com.abc.account

import org.scalatest.{Matchers, FlatSpec}
import com.abc.Account
import com.abc.Customer
import com.abc.BankException

class ExtendedMaxiSavingsAccountTest extends FlatSpec with Matchers {
 
  "ExtendedMaxiSavingsAccount" should "interest without withdraw in past 10 days" in {
    val extendedMaxiSavingsAccount: Account = new ExtendedMaxiSavingsAccount()
    extendedMaxiSavingsAccount.deposit(1000.0);
    extendedMaxiSavingsAccount.deposit(2000.0);
    extendedMaxiSavingsAccount.interestEarned() should be(150.0)
  }

  "it" should "interest with withdraw in past 10 days" in {
    val extendedMaxiSavingsAccount: Account = new ExtendedMaxiSavingsAccount()
    extendedMaxiSavingsAccount.deposit(1000.0);
    extendedMaxiSavingsAccount.deposit(2000.0);
    extendedMaxiSavingsAccount.deposit(3000.0);
    extendedMaxiSavingsAccount.withdraw(3000.0);
    extendedMaxiSavingsAccount.interestEarned() should be(3.0)
  }
    
}
