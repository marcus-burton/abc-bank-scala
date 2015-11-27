package com.abc

import scala.collection.mutable.ListBuffer
import AccountType._
import java.util.Calendar
import java.util.Date

class Account(accountType: AccountType) extends Format {
  import AccountType._
  import TransactionType._
  private val transactions: ListBuffer[Transaction] = ListBuffer()

  def deposit(amount: Double, date: Date = new Date()) {
    if (amount <= 0) throw new IllegalArgumentException("amount must be greater than zero")
    transactions += Transaction(DEPOSIT, amount, date)
  }

  def withdraw(amount: Double, date: Date = new Date()) {
    if (amount <= 0) throw new IllegalArgumentException("amount must be greater than zero")
    transactions += Transaction(WITHDRAW, amount, date)
  }

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    accountType match {
      case SAVINGS =>
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
      case MAXI_SAVINGS =>
        if (amount <= 1000) amount * 0.02
        else if (amount <= 2000) 20 + (amount - 1000) * 0.05
        else 70 + (amount - 2000) * 0.1
      case _ =>
        amount * 0.001
    }
  }

  def sumTransactions(): Double = transactions.map { x => if (x.transactionType == WITHDRAW) -x.amount else x.amount }.sum

  def statement: String = {
    val atype = accountType match {
      case CHECKING =>
        "Checking Account\n"
      case SAVINGS =>
        "Savings Account\n"
      case MAXI_SAVINGS =>
        "Maxi Savings Account\n"
    }
    val transactionSummary = transactions.map(t => transType(t) + " " + toDollars(t.amount.abs)).mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(sumTransactions)}"
    atype + transactionSummary + totalSummary
  }

  private def transType(t: Transaction) = t.transactionType match {
    case WITHDRAW => "withdrawal"
    case DEPOSIT  => "deposit"
    case _        => "interest"
  }

  def accrual(currentDate: Date = new Date()) {
    val percent2rate = Map(
      0.1 -> 0.00000274,
      0.2 -> 0.00000548,
      5.0 -> 0.00013689)

    val balance: Double = sumTransactions()
    val interest = accountType match {
      case CHECKING => balance * percent2rate(0.1)
      case SAVINGS =>
        if (balance <= 1000) balance * percent2rate(0.1)
        else 1000 * percent2rate(0.1) + (balance - 1000) * percent2rate(0.2)
      case MAXI_SAVINGS => 
        val ws = transactions.filter { _.transactionType == WITHDRAW }
        balance * (if(ws.isEmpty || olderThen(ws.last.date, currentDate, 10)) percent2rate(5.0) else percent2rate(0.1))
    }
    
    transactions += Transaction(INTEREST, interest, currentDate)
  }
  
  def olderThen(date: Date, currentDate: Date, num: Int) = {
    val d = Calendar.getInstance
    d.setTime(date)
    d.add(Calendar.DATE, num)
    
    val cd = Calendar.getInstance
    cd.setTime(currentDate)
    cd.clear(Calendar.HOUR)
    cd.clear(Calendar.MINUTE)
    cd.clear(Calendar.SECOND)
    cd.clear(Calendar.MILLISECOND)
    d.before(cd)
  }
}