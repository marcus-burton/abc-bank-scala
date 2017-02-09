package com.abc

import com.abc.AccountType.AccountType
import scala.collection.mutable.ListBuffer
import scala.util.{Success, Try}
// Should have created a Abstract base class or trait Account and then extended
// various accounts types from it.


class Account(var balance: BigDecimal = BigDecimal(0.0000)
              , val acct_type: AccountType) {

  // This can be kept private
  private val transactions = new ListBuffer[Transaction]()
  

  def deposit(amount: BigDecimal) = {
    amount match {
      case amount if (amount < 0) => throw new IllegalArgumentException(s"Trying to Deposit $amount which is negative amount")
      case _ => {
        balance = balance + amount;
        transactions += Transaction(amount, TxType.DEPOSIT)
      }
    }
  }

  // This is the same method as above but returns a Try collection so added a prefix "t_"
  def t_deposit(amount: BigDecimal): Try[BigDecimal] = {
    amount match {
      case amount if (amount < 0) => throw new RuntimeException(s"Trying to Deposit $amount which is negative amount")
      case _ => {
        balance = balance + amount;
        transactions += Transaction(amount, TxType.DEPOSIT);
        Success(amount)
      }
    }
  }


  def withdraw(amount: BigDecimal) = {
    amount match {
      case x if (x < 0) => throw new RuntimeException(s"Trying to Withdraw $x which is negative amount")
      case x if (x > balance) => throw new RuntimeException(s"Trying to Withdraw $x which is greater than balance $balance")
      case _ => {
        balance = balance - amount;
        transactions += Transaction(amount, TxType.WITHDRAWAL);
        amount
      }
    }

  }

// Same as withdraw but returns a Try[T]
  def t_withdraw(amount: BigDecimal): Try[BigDecimal] = {
    amount match {
      case x if x < 0 => throw new RuntimeException(s"Trying to Withdraw $x which is negative amount")
      case x if x > balance => throw new RuntimeException(s"Trying to Withdraw $x which is greater than $balance")
      case _ => {
        balance = balance - amount;
        transactions += Transaction(amount, TxType.WITHDRAWAL);
        Success(amount)
      }
    }

  }


  def print_account_details = {
    val AccountDescrStr = s"Account Type is $acct_type and Balance is ${df.format(balance)}"
    println(AccountDescrStr)
  }

  def print_statement = {
    transactions.foreach(x => {
      println(x.TxDetailsString)
    })
  }

  // This the net balance after adding all Deposits and withdrawals this should be the same as balance.
  def balance_from_statement = {
    val balance_d = transactions.filter(_.TranType == TxType.DEPOSIT).map(_.amount).sum -
      transactions.filter(_.TranType == TxType.WITHDRAWAL).map(_.amount).sum
    (balance_d).setScale(2, BigDecimal.RoundingMode.HALF_UP)
    //BigDecimal(balance_d).setScale(2, BigDecimal.RoundingMode.UP)
  }


  /*
  **Checking accounts** have a flat rate of 0.1%
  * **Savings accounts** have a rate of 0.1% for the first $1,000 then 0.2%
  * **Maxi-Savings accounts** have a rate of 2% for the first $1,000 then 5% for the next $1,000 then 10%
   */
  def interestEarned: BigDecimal = {
    acct_type match {
      case AccountType.SAVINGS => {
        if (balance <= 1000) balance * 0.001
        else 1 + (balance - 1000) * 0.002
      }
      case AccountType.MAXI_SAVINGS => {
        if (balance <= 1000) balance * 0.02
        else if (balance <= 2000) 20 + (balance - 1000) * 0.05
        else
          70 + (balance - 2000) * 0.1
      }
      //Checking Account default
      case _ => balance * 0.001
    }
  }

  def compound_interest_on_balance = calcCompoundInterest(principal = balance)

}
