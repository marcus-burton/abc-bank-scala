package com.abc.account

import org.scalatest.{Matchers, FlatSpec}
import com.abc.Account
import com.abc.Customer
import com.abc.BankException
import com.abc.util.Utils

class CheckingAccountTest extends FlatSpec with Matchers {
 
  "CheckingAccount" should "interest ignored" in {
    val checkingAccount: Account = new CheckingAccount()

    checkingAccount.deposit(4.1);
    assert(checkingAccount.interestEarned() < 0.01) 
  }
  
  "it" should "checking multiple deposit" in {
     val checkingAccount: Account = new CheckingAccount()
     checkingAccount.deposit(200.5);
     checkingAccount.deposit(700.3);
     
     checkingAccount.getBalance should be(900.8)
     checkingAccount.getTransactions.size should be(2)
     Utils.toDollars(checkingAccount.interestEarned()) should be("$0.90")
  }

  "it" should "checking deposit and withdrawal" in {
    val checkingAccount: Account = new CheckingAccount()
     checkingAccount.deposit(100.0);
     checkingAccount.withdraw(60.0);
     
     checkingAccount.getBalance should be(40.0)
  }

  "it" should "checking without enough fund for withdrawal" in {
    val checkingAccount: Account = new CheckingAccount()
     checkingAccount.deposit(60.0);
     intercept[BankException] { 
       checkingAccount.withdraw(100.0);
     }
  }

  "it" should "catch exception if depositing negative amount" in {
    val checkingAccount: Account = new CheckingAccount()
    
    intercept[BankException] { 
      checkingAccount.deposit(-290);
    }
  }

}
