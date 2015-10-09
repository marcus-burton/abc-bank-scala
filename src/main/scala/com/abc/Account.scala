package com.abc

import scala.collection.JavaConverters._
import java.util.{ArrayList, Collections => JavaCollections}

sealed abstract class AccountType(val name: String)
case object CHECKING extends AccountType("checking")
case object SAVINGS extends AccountType("savings")
case object MAXI_SAVINGS extends AccountType("maxi_savings")

class Account(val accountType: AccountType) {

  // By way of discussion real data would be stored somewhere on disk/not just in memory
  // When keeping a representation in memory the nice scala-ish thing to do is use immutable data structures
  // When data needs to be updated your options include using java synchronization methods, or using akka...
  // Using some kind of explicit synchronization is the smallest departure from whats here already.
  // Disclaimer: I haven't used scala conversions/syntactic sugar on of java synchronization wrappers before this.
  val transactions = JavaCollections.synchronizedList(new ArrayList[Transaction]())

  def deposit(amount: Double): Either[String, Boolean] =
    if (amount <= 0)
      Left("amount must be greater than zero")
    else
      Right(transactions.add(Transaction(amount)))

  def withdraw(amount: Double): Either[String, Boolean] =
    if (amount <= 0)
      Left("amount must be greater than zero")
    else
      Right(transactions.add(Transaction(-amount)))

  def interestEarned: Double = {
    val amount: Double = sumTransactions()
    if (amount <= 0)
      0
    else 
      accountType match {
        case CHECKING => amount * 0.001
        case SAVINGS =>
          if (amount <= 1000)
            amount * 0.001
          else
            1 + (amount - 1000) * 0.002
        case MAXI_SAVINGS =>
          if (amount <= 1000)
            amount * 0.02
          else if (amount <= 2000)
            20 + (amount - 1000) * 0.05
          else
            70 + (amount - 2000) * 0.1
    }
  }

  // not sure what the parameter is for. Either needs to be used or removed!
  def sumTransactions(/*checkAllTransactions: Boolean = true*/): Double = transactions.synchronized {
    transactions.asScala.map(_.amount).sum
  }

  def transactionSummary(formatter: Transaction => String): String = transactions.synchronized {
    transactions.asScala.map(formatter).mkString("  ", "\n  ", "\n")
  }

}