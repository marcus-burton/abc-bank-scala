package com.abc

import com.abc.AccountType.AccountType
/**
  * Created by Ravi on 2/7/2017.
  */

object BankMainApp extends App {

  println("Starting Account App")

  val Acc1 = new Account(acct_type = AccountType.CHECKING)
  Acc1.print_account_details

  Acc1.deposit(102309482.000)
  Acc1.withdraw(93.121)

  Acc1.print_account_details

  println("Now test Transactions")

  val t1 = Transaction(10, TranType = TxType.DEPOSIT, TimeStamp = getCurrentTime)


  println(t1.TxDetailsString)

  Acc1.print_statement

  println("Balance from Statement = " + Acc1.balance_from_statement)


  val bank1 = new Bank()
  val c1 = new Customer("Alpha")
  println(bank1.customerSummary)
  bank1.addCustomer(c1)
  c1.openAccount(100, AccountType.CHECKING)

  println(bank1.customerSummary)

  val c2 = new Customer("Beta")
  bank1.addCustomer(c2)


  c1.openAccount(200, AccountType.SAVINGS)

  c2.openAccount( 300, AccountType.SAVINGS)
  c1.print_balance_sheet


  println(bank1.customerSummary)


  println("Number of Accounts in C1 = " + c1.getNumberOfAccounts)


  println("Now transfer between accounts")
  c1.transferBetweenAccounts(AccountType.SAVINGS, AccountType.CHECKING, 50.0)

  c1.print_balance_sheet

}
