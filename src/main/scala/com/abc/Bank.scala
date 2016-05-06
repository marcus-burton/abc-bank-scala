package com.abc

import Account._

case class Bank(name: String, transactions: List[Transaction] = List()) {
 
   def doTransfer(accFrom: Account, accTo: Account, amount: Double, time: Int): Bank =
     this.copy(transactions = 
        Transaction(accFrom,-amount,time)::
        Transaction(accTo,amount,time)::
        transactions)

   def deposit(acc: Account, amount: Double, time: Int): Bank =
        this.copy(transactions = 
        Transaction(acc,amount,time)::
        transactions)
  
   def addAccount(account: Account, time: Int): Bank = {
      this.copy(transactions = Transaction(account,0,time)::transactions)
   }
    
  def customerReport: String = {
    var out=""
    val rep = transactions.groupBy(_.account.owner).map {
      case (key, value) => (key, value.map(_.amount).sum)
     }.foreach {
           case (name, total) =>
           println(s"$name, $total")
           out += s"$name, $total"
      }
  out
  }
 
 def interest(acctType: Account, amount: Double): Double = acctType match {
    case _: Checking => checkingInterest(amount)
    case _: Savings => savingsInterest(amount)
    case _: MaxSavings => maxSavingsInterest(amount)
  }
  
  def checkingInterest(amount: Double) = {
    require(amount >= 0)
    amount * 0.001
  }
  
  def savingsInterest(amount: Double): Double = {
    require(amount >= 0)
    if (amount <= 1000) 
      amount * 0.001
    else
      savingsInterest(1000) + (amount - 1000) * 0.002
  }
  
  def maxSavingsInterest(amount: Double): Double = {
    require(amount >= 0) 
    if (amount <= 1000) 
      amount * 0.02
    else if (amount > 1000 && amount <= 2000) 
      maxSavingsInterest(1000) + (amount - 1000) * 0.05
    else
      maxSavingsInterest(2000) + (amount - 2000) * 0.1
  }
}


