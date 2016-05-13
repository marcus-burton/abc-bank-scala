package com.abc

import Account._

case class Bank(name: String, transactions: List[Transaction] = List()) {

  val (checking,savings,maxi) = (0,1,2)

  def doTransfer(accountFrom: Account, accountTo: Account, amount: Double, time: Int): Bank =
    this.copy(transactions = transactions :+ Transaction(accountFrom,-amount,time) :+ Transaction(accountTo,amount,time))

  def deposit(account: Account, amount: Double, time: Int): Bank =
    this.copy(transactions = transactions :+ Transaction(account,amount,time))

  def addAccount(account: Account, time: Int): Bank =
    this.copy(transactions = transactions :+ Transaction(account,0,time))

  def customerReport(reportTime: Int) : String = {
    var outCurrent, out = ""
    val rep = transactions.groupBy(_.account.owner).map { case (customerName, transactions) =>
        val total, intOnInt = Array.fill[Double](3)(0.0)
        var hasChecking, hasSavings, hasMaxiS = false
        var i, daysSinceLastWithdrawal = 0
        val transIter = transactions.toIterator
        var currentTrans = transIter.next()

        while (i < reportTime) {
          if (i == currentTrans.time) {
            val accountType = currentTrans.account match {
              case _: Checking => hasChecking=true; checking
              case _: Savings => hasSavings=true; savings
              case _: MaxSavings => hasMaxiS=true; maxi
            }
            total(accountType) += currentTrans.amount
            if (currentTrans.amount < 0) daysSinceLastWithdrawal = 0
            if (transIter.hasNext) {currentTrans = transIter.next; if (i==currentTrans.time) i-=1}
          }

          for (accountType <- List(checking, savings, maxi))
            intOnInt(accountType) += interest(accountType,total(accountType)+intOnInt(accountType),daysSinceLastWithdrawal)

          i += 1; daysSinceLastWithdrawal += 1
        }
        (customerName, hasChecking,total(checking)+intOnInt(checking),intOnInt(checking),
                hasSavings,total(savings)+intOnInt(savings),intOnInt(savings),
                hasMaxiS,total(maxi)+intOnInt(maxi),intOnInt(maxi))
    }.foreach { case (name,hasC,totalC,intC,hasS,totalS,intS,hasMaxiS,totalMaxS,intM) =>
        outCurrent = "\n"
        if(hasC) outCurrent += s"$name has checking account  with current balance = $totalC, including accrued interest = $intC\n"
        if(hasS) outCurrent += s"$name has savings account  with current balance = $totalS, including accrued interest = $intS\n"
        if(hasMaxiS) outCurrent += s"$name has maxi account  with current balance = $totalMaxS, including accrued interest = $intM\n"
        println(outCurrent); out+=outCurrent
    }
    out
  }
 
 def interest(accountType: Int, amount: Double, daysSinceLastWithdrawal: Int): Double = accountType match {
    case `checking` => checkingInterest(amount)
    case `savings` => savingsInterest(amount)
    case `maxi` => maxSavingsInterest(amount, daysSinceLastWithdrawal)
  }
  
  def checkingInterest(amount: Double) = {
    require(amount >= 0)
    amount * 0.01/365
  }
  
  def savingsInterest(amount: Double): Double = {
    require(amount >= 0)
    if (amount <= 1000) 
      amount * 0.01/365
    else
      savingsInterest(1000) + (amount - 1000) * 0.02/365
  }
  
  def maxSavingsInterest(amount: Double, daysSinceLastWithdrawal: Int): Double = {
    val rate = if (daysSinceLastWithdrawal > 10) 0.1/365 else 0.05/365
    require(amount >= 0) 
    if (amount <= 1000) 
      amount * 0.02/365
    else if (amount > 1000 && amount <= 2000) 
      maxSavingsInterest(1000, daysSinceLastWithdrawal) + (amount - 1000) * rate
    else
      maxSavingsInterest(2000, daysSinceLastWithdrawal) + (amount - 2000) * rate
  }
}


