package com.abc

import Account._

case class Bank(name: String, transactions: List[Transaction] = List()) {
 
   def doTransfer(accFrom: Account, accTo: Account, amount: Double, time: Int): Bank =
     this.copy(transactions = transactions :+
        Transaction(accFrom,-amount,time) :+
        Transaction(accTo,amount,time))

   def deposit(acc: Account, amount: Double, time: Int): Bank =
        this.copy(transactions =
          transactions :+ Transaction(acc,amount,time))
  
   def addAccount(acc: Account, time: Int): Bank = {
      this.copy(transactions = transactions :+ Transaction(acc,0,time))
   }

  def customerReport(ctime: Int) : String = {
    var out = ""
    val rep = transactions.groupBy(_.account.owner).map {
      case (key, value) =>
        var total = 0.0
        val i1 = value.toIterator
        var i = 0
        var j = 0 // numbers of days since last withdrawal
        var l1 = i1.next()
        while (i < ctime) {
          if (i == l1.time) {
            total += l1.amount
            if (l1.amount < 0) j = 0
            if (i1.hasNext) l1 = i1.next()
          }
          total += interest(l1.account,total,j)
          i += 1
          j += 1
        }
        (key,total)
    }.foreach {
      case (name, total) =>
        println(s"$name, $total")
        out += s"$name, $total"
    }
    out
  }
 
 def interest(acctType: Account, amount: Double, lw: Int): Double = acctType match {
    case _: Checking => checkingInterest(amount)
    case _: Savings => savingsInterest(amount)
    case _: MaxSavings => maxSavingsInterest(amount, lw)
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
  
  def maxSavingsInterest(amount: Double, lw: Int): Double = {
    val intr = if (lw > 10) 0.1 else 0.05
    require(amount >= 0) 
    if (amount <= 1000) 
      amount * 0.02
    else if (amount > 1000 && amount <= 2000) 
      maxSavingsInterest(1000, lw) + (amount - 1000) * intr
    else
      maxSavingsInterest(2000, lw) + (amount - 2000) * intr
  }
}


