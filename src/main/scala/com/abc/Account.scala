package com.abc

import scala.collection.mutable.ListBuffer
import java.util.Date
import java.util.Calendar

object Account {

  /*
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
  */
  //val's are immutable
  val CHECKING: Int = 0
  val SAVINGS: Int = 1
  val MAXI_SAVINGS: Int = 2
  
}

class Account(val accountType: Int, var transactions: ListBuffer[Transaction] = ListBuffer()) {

  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(1, amount) //tranType:1 -> deposit     
  }

  def withdraw(amount: Double) {    
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else if (amount > sumTransactions()) //check for insufficient funds
      throw new IllegalArgumentException("insufficient funds") 
    else  
      transactions += Transaction(0, -amount) //tranType:0 -> withdrawal      
  }
  
  /**
   * Additional feature -> A customer can transfer between their accounts.
   * In real world scenario this needs to be handled in asynchronous fashion to 
   * avoid any data integrity/concurrency issues.
   */
  def transfer(fromAcct: Account, toAcct: Account, transferAmt: Double) = {
    if (transferAmt <= 0)
      throw new IllegalArgumentException("transfer amount must be greater than zero")
    else if (transferAmt > fromAcct.sumTransactions()) 
      throw new IllegalArgumentException("insufficient funds, cannot transfer") 
    else
      fromAcct.withdraw(transferAmt)
      toAcct.deposit(transferAmt)  
  }   
  
  def interestEarned: Double = {
    val balanceAmt: Double = sumTransactions()
    accountType match {
      case Account.SAVINGS =>       
        /*
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
        */
        //factored out all interest calculations to individual functions
        savingsInterestEarned(balanceAmt)
        
      case Account.MAXI_SAVINGS =>
        /*
        if (amount <= 1000) return amount * 0.02
        if (amount <= 2000) return 20 + (amount - 1000) * 0.05
        70 + (amount - 2000) * 0.1
        */
        maxiSavingsInterestEarned(balanceAmt)
        
      case _ =>
        //amount * 0.001
        checkingInterestEarned(balanceAmt)
    }
  }
  
  def checkingInterestEarned(balanceAmt: Double): Double = {
    balanceAmt * 0.001
  }  

  def savingsInterestEarned(balanceAmt: Double): Double = {
    if (balanceAmt <= 1000) balanceAmt * 0.001
    else 1 + (balanceAmt - 1000) * 0.002
  }
  
  /**
   * Additional feature -> Maxi-Savings accounts: to have an interest rate of 5% 
   * assuming no withdrawals in the past 10 days otherwise 0.1%
   */  
  def maxiSavingsInterestEarned(balanceAmt: Double): Double = {
  
    balanceAmt match {    
      case ba if (ba <= 1000) => 
        balanceAmt * 0.02
          
      case ba if (ba <= 2000) => 
        if (isAnyWithdrawalsInPastTenDays) 20 + (balanceAmt - 1000) * 0.001 
        else 20 + (balanceAmt - 1000) * 0.05
      
      case _ => 
        if (isAnyWithdrawalsInPastTenDays) 21 + (balanceAmt - 2000) * 0.1 
        else 70 + (balanceAmt - 2000) * 0.1
    }            
  }
    
  /**
   * To find out any withdrawals in the past 10 days from 
   * account transactions list.
   */    
  def isAnyWithdrawalsInPastTenDays: Boolean = {
     val currDate = DateProvider.now 
     if (transactions.filter(t => (t.tranType == 0 && daysBetween(t.tranDate, currDate) <= 10)).size >= 1) true
     else false 
  }
  
  /**
   * To find out no. of days between current and transaction date.
   */      
  def daysBetween(tranDate: Date, currDate: Date): Long = {
   (currDate.getTime - tranDate.getTime) / (1000 * 60 * 60 * 24)  
  }
   
  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

}