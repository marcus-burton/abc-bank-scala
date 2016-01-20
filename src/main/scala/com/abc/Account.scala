package com.abc

import scala.collection.mutable.ListBuffer

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
      transactions += Transaction(amount)
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount)
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) amount * (0.001/365)
        else 1 + (amount - 1000) * (0.002/365)
      case Account.MAXI_SAVINGS =>
        var intRate:Double = 0.0
        if(withdrawnInLast10Days)
        {
          intRate = 0.001
        }
        else
        {
          intRate = 0.05
        }

        amount * (intRate/365)

//        if (amount <= 1000) return amount * (0.02/365)
//        if (amount <= 2000) return 20 + (amount - 1000) * (0.05/365)
//        70 + (amount - 2000) * (0.1/365)

      case _ =>
        amount * (0.001/365)
    }
  }



  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

  def transfer(fromAccount:Account, amount: Double): Unit =
  {
    this.synchronized {
      fromAccount.synchronized {
        if(fromAccount.sumTransactions() - amount > 0) // from account should have sufficient funds to transfer
          {
            fromAccount.withdraw(amount)
            deposit(amount)
          }
        else{
          throw new Exception("insufficient funds available in the account")
        }
       }
    }
  }

  def withdrawnInLast10Days()  :Boolean =
  {

    if(transactions.filter(t => t.transactionDate.after(DateProvider.getInstance.now_minus_10 ) ).length> 0)
    {
      true
    }

    false
  }


}