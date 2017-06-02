package com.abc

import org.scalatest.{Matchers, FlatSpec}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class CustomerTest extends FlatSpec with Matchers {
  "Customer" should "statement" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    val savingsAccount: Account = new Account(Account.SAVINGS)
    val henry: Customer = new Customer("Henry").openAccount(checkingAccount).openAccount(savingsAccount)
    checkingAccount.deposit(100.0)
    savingsAccount.deposit(4000.0)
    savingsAccount.withdraw(200.0)
    henry.getStatement should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  it should "testOneAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.numberOfAccounts should be(1)
  }

  it should "testTwoAccount" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.openAccount(new Account(Account.CHECKING))
    oscar.numberOfAccounts should be(2)
  }

  it should "testThreeAcounts" in {
    val oscar: Customer = new Customer("Oscar").openAccount(new Account(Account.SAVINGS))
    oscar.openAccount(new Account(Account.CHECKING))
    oscar.openAccount(new Account(Account.MAXI_SAVINGS))
    oscar.numberOfAccounts should be(3)
  }

  it should "internal transfer correct" in {
    val checkingAct = new Account(Account.CHECKING)
    val savingAct = new Account(Account.SAVINGS)
    val oscar: Customer = new Customer("Oscar").openAccount(checkingAct)
    oscar.openAccount(savingAct)
    checkingAct.deposit(100)
    savingAct.deposit(30)
    oscar.internalTransfer(checkingAct, savingAct, 10)
    checkingAct.balance should be (90)
    savingAct.balance should be (40)
    checkingAct.transactions(1).amount should be (-10)
    savingAct.transactions(1).amount should be (10)
  }


  it should "internal transfer correct at the same time" in {
      val checkingAct = new Account(Account.CHECKING)
      val savingAct = new Account(Account.SAVINGS)
      val oscar: Customer = new Customer("Oscar").openAccount(checkingAct)
      oscar.openAccount(savingAct)
      checkingAct.deposit(100)
      savingAct.deposit(100)
      val t1 = Future {
        oscar.internalTransfer(checkingAct, savingAct, 10)
      }
      val t2 = Future {
        oscar.internalTransfer(savingAct, checkingAct, 20)
      }
      t1.onSuccess {
        case _ => t2.onSuccess {
          case _ => {
            checkingAct.balance should be(110)
            savingAct.balance should be(90)
            checkingAct.transactions(2).amount should be(20)
            savingAct.transactions(2).amount should be(-20)
          }
        }
      }
  }

}
