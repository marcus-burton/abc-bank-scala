package com.abc

import scala.collection.mutable.ListBuffer
import java.util.Calendar
import java.util.Date

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
  
  final val SAVINGS_UPGRADE_CHECK_DAYS = 10
  
  def intereastEarnedYearly (amount: Double, accountType: Int): Double = accountType match {
      case SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case MAXI_SAVINGS =>
        if (amount <= 1000) return amount * 0.02
        if (amount <= 2000) return 20 + (amount - 1000) * 0.05
        70 + (amount - 2000) * 0.1
      case _ =>
        amount * 0.001 
  }
 
  def intereastEarnedDaily (amount: Double, accountType: Int): Double = intereastEarnedYearly (amount, accountType) / 365
  
  def round(a: Double) = (math rint a * 100) / 100
  
  def diffDays(date1: Date, date2: Date):Int = ((date2.getTime - date1.getTime) / 1000 / 60 / 60 / 24).toInt

  
}

class Account(val accountType: Int, var transactions: ListBuffer[Transaction] = ListBuffer()) {
  
  import Account._

  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(amount)
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount)
  }

  def interestEarnedByYear: Double = {
    val amount: Double = sumTransactions()
    Account.intereastEarnedYearly(amount, accountType)
  }
  
 // def interestEarned(): Double =   interestEarnedOnDate(new Date)
   
  
  def interestEarned (date: Date = DateProvider.getInstance.now): Double = {
    
    if (diffDays(DateProvider.getInstance.now, date) < 0) throw new Error("Not supported")
       
        
    def trxDays(x: Date, dates: List[Date]): List[Int] = dates match {
      case List() => List()
      case y :: ys =>  diffDays(x, y) ::  trxDays (y, ys)
    }    
    def trxDaysCount (dates: List[Date]): List[Int] = dates match {
      case List() => List ()
      case y :: ys => trxDays(y, ys)
    }

    val trxDates = transactions.map(x => x.transactionDate).toList :+ date

    val days: List[Int] = trxDaysCount(trxDates)
    
    val trxAmounts = transactions.map(x => x.amount).toList
    
    val amountDaysPair = days zip trxAmounts  
    
    val earns = amountDaysPair.map(x => Account.intereastEarnedDaily(x._1 * x._2, accountType))

    round(earns.sum)
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

  def upgradeSavingAccount(date: Date = DateProvider.getInstance.now): Account =
 

    if (diffDays(DateProvider.getInstance.now, date) < 0) this
    else{
         println(s"original one $transactions")
         val trxs = transactions.clone.toList
      accountType match {

        case CHECKING | MAXI_SAVINGS => this
        case SAVINGS => {

          def checkTransactionAmounts: List[Double] = {
            val cal = Calendar.getInstance
            cal.setTime(date)
            cal.add(Calendar.DATE, -SAVINGS_UPGRADE_CHECK_DAYS)
            val checkBackDate = cal.getTime
            val checkTransections = trxs.filter(x => x.transactionDate.after(checkBackDate)).toList   
            val amounts = checkTransections.map(x => x.amount).toList
            amounts
          }

          def withdrawn(x: Double, amounts: List[Double]): Boolean = amounts match {
            case List() => false
            case y :: ys => if (x <= y) withdrawn(y, ys) else true
          }

          def hasWithdraw(amount: List[Double]): Boolean = amount match {
            case List() => false
            case y :: ys => withdrawn(y, ys)
          }

          def hasBalence = sumTransactions() > 0

          val newAccount: Account = if (hasBalence && hasWithdraw(checkTransactionAmounts)) this
          else new Account(MAXI_SAVINGS, transactions)

          newAccount
        }

      }
    }

}