package com.abc

import scala.collection.mutable.ListBuffer
import java.util.Calendar

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

class Account(val accountType: Int, var transactions: ListBuffer[Transaction] = ListBuffer()) {

  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction("deposit", amount)
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction("withdrawal", -amount)
  }

  def transfer(to: Account, amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction("transfer", -amount)
      to.transactions += Transaction("transfer", amount)
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    accountType match {
      case Account.CHECKING =>
        val now = Calendar.getInstance()
        val amountByDay = transactions.groupBy(t => dateDifferenceDays(t.transactionDate, now)).map { case (k,v) => k -> (v map (_.amount) sum) }
        amountByDay.map(da => da._2 * (math.pow(1+0.001/365, da._1)-1)).sum

      case Account.SAVINGS =>
        val now = Calendar.getInstance()
        val amountByDay = transactions.groupBy(t => dateDifferenceDays(t.transactionDate, now)).map { case (k,v) => k -> (v map (_.amount) sum) }

        var accu: Double = 0.00
        var dayAccu: ListBuffer[(Int, Double)] = ListBuffer()
        for(da <- amountByDay) {
          accu += da._2
          dayAccu += ((da._1, accu))
        }
        dayAccu.map(da => {
          if(da._2 <= 1000)
            da._2 * (math.pow(1+0.001/365, da._1)-1)
          else
            1000 * (math.pow(1+0.001/365, da._1)-1) + (da._2-1000) * (math.pow(1+0.002/365, da._1)-1)
        }).sum

      case Account.MAXI_SAVINGS => {
        val cal = Calendar.getInstance()
        cal.add(Calendar.DATE, -10)
        val recentWithdrawals = transactions.filter(_.transactionDate.getTime.after(cal.getTime)).filter(_.method=="withdrawal")

        val now = Calendar.getInstance()
        val amountByDay = transactions.groupBy(t => dateDifferenceDays(t.transactionDate, now)).map { case (k,v) => k -> (v map (_.amount) sum) }
        println(recentWithdrawals)
        if (recentWithdrawals.isEmpty)
          amountByDay.map(da => da._2 * (math.pow(1+0.05/365, da._1)-1)).sum
        else
          amountByDay.map(da => da._2 * (math.pow(1+0.001/365, da._1)-1)).sum
      }
    }
  }

  def transactionDateAmount(checkAllTransactions: Boolean = true) = {

    val date = transactions.map(_.transactionDate)
    val amount = transactions.map(_.amount)
    date zip amount

  }

  def dateDifferenceDays(c1: Calendar, c2: Calendar): Int = {
    c1.set(Calendar.MILLISECOND, 0)
    c1.set(Calendar.SECOND, 0)
    c1.set(Calendar.MINUTE, 0)
    c1.set(Calendar.HOUR_OF_DAY, 0)
    c2.set(Calendar.MILLISECOND, 0)
    c2.set(Calendar.SECOND, 0)
    c2.set(Calendar.MINUTE, 0)
    c2.set(Calendar.HOUR_OF_DAY, 0)

    var diff: Int = 1
    while (c1.before(c2))
    {
      diff+=1
      c1.add(Calendar.DATE, +1)
    }
    diff
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

}