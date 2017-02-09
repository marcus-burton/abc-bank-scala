package com.abc

import com.abc.AccountType.AccountType
import com.abc.Account._

import scala.collection.mutable.ListBuffer
import scala.util.Try
/**
  * Created by Ravi on 2/5/2017.
  */

class Customer(val name: String) {

  // Assumption each customer can have only one three types of accounts
  // changed account list from var to val
  private val accounts = new ListBuffer[Account]()

  def openAccount(opening_balance: BigDecimal, accountType: AccountType) = {
    // check if the account type already exists in the list
    if (accounts.exists(x => x.acct_type == accountType)) throw new RuntimeException(s"Trying to Open an Account of type $accountType which already exists for $name")
    else
      accounts += new Account(opening_balance, accountType)
  }

  def getNumberOfAccounts = accounts.size

  def totalInterestEarned = accounts.map(_.interestEarned).sum

  def totalCompoundInterestEarned = accounts.map(_.compound_interest_on_balance).sum

  def totalBalanceOverAllAccounts = accounts.map(_.balance).sum

  def print_balance_sheet = accounts.foreach(x => println(f"$name%-10s  ${x.acct_type}%10s ${x.balance}%10.2f "))

  def transferBetweenAccounts(src_acct: AccountType, dest_acct: AccountType, amount: BigDecimal): Try[BigDecimal] = {
    // 1) We just need the head since there is only one of each type of account
    //    First Step check whether each of those accounts exist - throw an Exception otherwise
    //    filter.headOption can be replaced with find - but will have to test everything again.
    /*val src_account = accounts.filter(_.acct_type == src_acct).headOption.
      getOrElse(throw new RuntimeException(s"$src_acct does not exist"))
    val dest_account = accounts.filter(_.acct_type == dest_acct).headOption.
      getOrElse(throw new RuntimeException(s"$dest_acct does not exist"))*/

    val src_account = accounts.find(_.acct_type == src_acct).
      getOrElse(throw new RuntimeException(s"$src_acct does not exist"))
    val dest_account = accounts.find(_.acct_type == dest_acct).
      getOrElse(throw new RuntimeException(s"$dest_acct does not exist"))

    // 2) if the source and destination accounts exist - we need to make sure that they are different types
    //    e.g. Depositing from SAVINGS to SAVINGS Account does not make sense.
    //    while this is superfluous its a still a good check.

    if (src_account.acct_type == dest_account.acct_type)
      (throw new RuntimeException(s"transfer error source $src_account.acct_type is same as Destination $dest_account.acct_type"))

    // 3) Now do the transfer
    src_account.t_withdraw(amount).flatMap(dest_account.t_deposit(_))
  }


}




