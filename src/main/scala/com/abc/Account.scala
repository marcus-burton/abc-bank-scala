package com.abc

import scala.collection.mutable.ArrayBuffer

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

case class Account(val accountType: Int, val transactions: ArrayBuffer[Transaction] = ArrayBuffer()) {
val balance:AtomicDouble=new AtomicDouble(0)
  def transfer(from:Account,amount:Double){
  if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else {
  synchronized{
    this.deposit(amount)
    from.withdraw(amount)
  }
    }
}
  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else {
      this.synchronized{
      transactions += new Transaction(amount)
      }
      balance.addAndGet(amount)
    }
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    if(balance.doubleValue()  -amount <= 0)
      throw new InsufficientFundsException();
    else {
      this.synchronized {
      transactions += new Transaction(-amount)
      } 
      balance.addAndGet(-amount)
    }
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    accountType match {
      case Account.SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
        /**
      case Account.MAXI_SAVINGS =>
        if (amount <= 1000) return amount * 0.02
        if (amount <= 2000) return 20 + (amount - 1000) * 0.05
        70 + (amount - 2000) * 0.1
        
        */
      case Account.MAXI_SAVINGS => 
        if(checkWithdrawalsInLastTenDays){
          return amount * 0.001
        }
        else {
          amount * 0.05
        }
      case Account.CHECKING =>
        amount * 0.001
    }
  }
  def checkWithdrawalsInLastTenDays():Boolean ={
    val dateProvider:DateProvider =new DateProvider()
    for(txn <- transactions){
      if(txn.amount <=0 && dateProvider.daysBetween(txn.transactionDate) <=10)
      return true
    }
    return false
  }
  def sumTransactions(checkAllTransactions: Boolean = true): Double = balance.doubleValue

}