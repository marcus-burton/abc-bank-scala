package com.abc

import org.scalatest.{Matchers, FlatSpec}
import com.abc.account.CheckingAccount
import com.abc.account.SavingsAccount

class CustomerTest extends FlatSpec with Matchers {
 
  "Customer" should "statement" in {
    val checkingAccount: Account = new CheckingAccount()
    val savingsAccount: Account = new SavingsAccount()
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "construct a Customer with passing null account" in {
    intercept[BankException] { 
      val tom: Customer = new Customer("Tom", null)
    }
  }
  
  it should "testOneAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new SavingsAccount())
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new SavingsAccount())
    oscar.openAccount(new CheckingAccount())
    oscar.numberOfAccounts should be(2)
  }

  ignore should "testThreeAcounts" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new SavingsAccount())
    oscar.openAccount(new CheckingAccount())
    oscar.numberOfAccounts should be(3)
  }
  
  it should "open account without providing account" in {
    val oscar: Customer = new Customer("Oscar")
    
    intercept[BankException] { 
      oscar.openAccount(null)
    }
  }

  it should "open duplicated account" in {
    val oscar: Customer = new Customer("Oscar")
    oscar.openAccount(new SavingsAccount())
    
    intercept[BankException] { 
      oscar.openAccount(new SavingsAccount())
    }
	}
  
  it should "test customer account transfer" in {

        val checkingAccount: Account = new CheckingAccount();
        val savingsAccount: Account  = new SavingsAccount();

        val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount);

        checkingAccount.deposit(5000.0);
        savingsAccount.deposit(100.0);
        
        henry.transfer(checkingAccount, savingsAccount, 200.0);
        
        checkingAccount.getBalance should be(4800.0)
        savingsAccount.getBalance should be(300.0)        
    }

    it should "test customer same account transfer" in {

        val checkingAccount: Account = new CheckingAccount();
        val checkingAccount2: Account = new CheckingAccount();

        val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(new SavingsAccount)

        checkingAccount.deposit(5000.0);
        
        intercept[BankException] {
          henry.transfer(checkingAccount, checkingAccount2, 200.0);
        }        
    }

  
}
