package com.abc

import Account._

case class Bank(name: String, var transactions: List[Transaction] = List()) {

  val checking:Int = 0
  val savings:Int = 1
  val maxi:Int = 2

  def doTransfer(accFrom: Account, accTo: Account, amount: Double, time: Int): Bank =
    this.copy(transactions = transactions :+ Transaction(accFrom,amount,time) :+ Transaction(accTo,amount,time+1))

  def deposit(acc: Account, amount: Double, time: Int): Bank =
    this.copy(transactions = transactions :+ Transaction(acc,amount,time))
  
  def addAccount(acc: Account, time: Int): Bank =
    this.copy(transactions = transactions :+ Transaction(acc,0,time))


  def customerReport(ctime: Int) : String = {
    var out = ""
    val rep = transactions.groupBy(_.account.owner).map {
      case (key, value) =>
        val total = Array.fill[Double](3)(0.0)
        val i1 = value.toIterator
        var accType, i,k,j = 0 // j - numbers of days since last withdrawal
        var l1 = i1.next()

        while (i < ctime) {
          if (i<value.last.time+1 && i==l1.time) {
            accType = l1.account match {
              case _: Checking => checking
              case _: Savings => savings
              case _: MaxSavings => maxi
            }
            total(accType) += l1.amount
            if (l1.amount < 0) {k=j;j=0}
            if (i1.hasNext) l1 = i1.next
            if (i == l1.time) {
              accType = l1.account match {
                case _: Checking => checking
                case _: Savings => savings
                case _: MaxSavings => maxi
              }
              total(accType) += l1.amount
              j=k
            }
          }

          total(checking) += interest(checking,total(checking),j)
          total(savings) += interest(savings,total(savings),j)
          total(maxi) += interest(maxi,total(maxi),j)
          i += 1
          j += 1
        }
        (key,total(checking),total(savings),total(maxi))
    }.foreach {
      case (name, totalC, totalS, totalMaxS) =>
        println(s"$name, Checking=$totalC,Savings=$totalS, MaxSavings=$totalMaxS")
        out += s"$name, Checking=$totalC,Savings=$totalS, MaxSavings=$totalMaxS"
    }
    out
  }
 
 def interest(acctType: Int, amount: Double, lw: Int): Double = acctType match {
    case `checking` => checkingInterest(amount)
    case `savings` => savingsInterest(amount)
    case `maxi` => maxSavingsInterest(amount, lw)
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


