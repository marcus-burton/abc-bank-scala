package com.abc

import org.joda.time.DateTime
import org.scalatest.{Matchers, FlatSpec}

class BankTest extends FlatSpec with Matchers {

    "Bank" should "customer summary" in {
      val bank: Bank = new Bank
      val john: Customer = new Customer("John").openAccount(new Account(Account.CHECKING))
      bank.addCustomer(john)
      bank.customerSummary should be("Customer Summary\n - John (1 account)")
    }

    it should "checking account with real days" in {
      val bank: Bank = new Bank
      val checkingAccount: Account = new Account(Account.CHECKING)
      bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
      val testDay1 = (new DateTime).withYear(2015).withMonthOfYear(11).withDayOfMonth(25) //create a fake transaction date, otherwise it's always 0.
      val testDay2 = (new DateTime).withYear(2015).withMonthOfYear(12).withDayOfMonth(25)
      val t1 = new Transaction(900.0, testDay1, "deposit")
      val t2 = new Transaction(900.0, testDay2, "deposit")
      checkingAccount.transactions += t1
      checkingAccount.transactions += t2
      //checkingAccount.deposit(3000.0)
      bank.totalInterestPaid should be(0.15)
    }

    it should "savings account with daily interest" in {
      val bank: Bank = new Bank
      val checkingAccount: Account = new Account(Account.SAVINGS)
      bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
      val testDay1 = (new DateTime).withYear(2015).withMonthOfYear(11).withDayOfMonth(25)
      val testDay2 = (new DateTime).withYear(2015).withMonthOfYear(12).withDayOfMonth(25)
      val t1 = new Transaction(900.0, testDay1, "deposit")
      val t2 = new Transaction(900.0, testDay2, "deposit")
      checkingAccount.transactions += t1
      checkingAccount.transactions += t2
      //checkingAccount.deposit(3000.0)
      bank.totalInterestPaid should be(0.13)
    }

    it should "maxi savings account with daily interest" in {
      val bank: Bank = new Bank
      val checkingAccount: Account = new Account(Account.MAXI_SAVINGS)
      bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
            val testDay1 = (new DateTime).withYear(2015).withMonthOfYear(11).withDayOfMonth(25)
            val testDay2 = (new DateTime).withYear(2015).withMonthOfYear(12).withDayOfMonth(25)
            val t1 = new Transaction(2000.0, testDay1, "deposit")
            //val t2 = new Transaction(200.0, testDay2, "deposit")
            //checkingAccount.transactions += t2
            checkingAccount.transactions += t1
      checkingAccount.withdraw(100.0)
      //checkingAccount.deposit(3000.0)
      bank.totalInterestPaid should be(21.17)
    }

   it should "maxi savings account with daily interest AND withdrawal in past 10 days" in {
     val bank: Bank = new Bank
     val checkingAccount: Account = new Account(Account.MAXI_SAVINGS)
     bank.addCustomer(new Customer("Bill").openAccount(checkingAccount))
     val testDay1 = (new DateTime).withYear(2015).withMonthOfYear(11).withDayOfMonth(25) //create a fake transaction date, otherwise it's always 0.
     val testDay2 = (new DateTime).withYear(2015).withMonthOfYear(12).withDayOfMonth(25)
     val t1 = new Transaction(4000.0, testDay1, "deposit")
     val t2 = new Transaction(4000.0, testDay2, "deposit")
     checkingAccount.transactions += t1
     checkingAccount.transactions += t2
     //checkingAccount.deposit(3000.0)
     bank.totalInterestPaid should be(49.89)
   }

    it should "return first customer; testOne" in {
    val bank: Bank = new Bank
    val craig: Customer = new Customer("Craig").openAccount(new Account(Account.MAXI_SAVINGS))
    bank.addCustomer(craig)
    bank.getFirstCustomer should be("Craig")
  }

  it should "testTwo" in {
    val bank: Bank = new Bank
    val craig: Customer = new Customer("Craig").openAccount(new Account(Account.MAXI_SAVINGS))
    val norbert: Customer = new Customer("Norbert").openAccount(new Account(Account.MAXI_SAVINGS))
    bank.addCustomer(craig)
    bank.addCustomer(norbert)
    bank.getFirstCustomer should be("Craig")
  }

}
