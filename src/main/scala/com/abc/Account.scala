package com.abc

import java.util.UUID

import org.apache.commons.lang3.builder.{EqualsBuilder, HashCodeBuilder}


abstract class Account(val transactions: List[Transaction] = List.empty,
                       val id: String = UUID.randomUUID().toString) {

  def interestEarned: Double
  def title: String
  def withTransactions(transactions: List[Transaction]): Account


  def deposit(amount: Double, daysOffSet: Int = 0): Account = {
    require(amount >= 0, "amount must be greater than zero")

    withTransactions(transactions = transactions :+ Transaction(amount, daysOffSet))
  }

  def withdraw(amount: Double, daysOffSet: Int = 0): Account = {
    require(amount >= 0, "amount must be greater than zero")

    withTransactions(transactions = transactions :+ Transaction(-amount, daysOffSet))
  }

  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum

  override def hashCode(): Int = {
    new HashCodeBuilder(17, 37).
      append(id).
      toHashCode
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that: Account if that.isInstanceOf[Account] => {
        val acct = obj.asInstanceOf[Account]
        new EqualsBuilder()
          .append(id, acct.id)
          .isEquals
      }
      case _ => false
    }
  }
}

case class Checking(override val transactions: List[Transaction] = List.empty,
                    override val id: String = UUID.randomUUID().toString) extends Account(transactions, id) {
  def withTransactions(transactions: List[Transaction]): Checking = {
    this.copy(transactions)
  }

  override def interestEarned: Double = sumTransactions() * 0.001

  override def title: String = "Checking Account"
}

case class Savings(override val transactions: List[Transaction] = List.empty,
                   override val id: String = UUID.randomUUID().toString) extends Account(transactions, id) {
  def withTransactions(transactions: List[Transaction]): Savings = {
    this.copy(transactions)
  }

  override def interestEarned: Double = {
    sumTransactions() match {
      case amount if (amount <= 1000) => amount * 0.001
      case amount => 1 + (amount - 1000) * 0.002
    }
  }

  def title: String = "Savings Account"
}

case class MaxiSavings(override val transactions: List[Transaction] = List.empty,
                       override val id: String = UUID.randomUUID().toString) extends Account(transactions, id) {

  def withTransactions(transactions: List[Transaction]): MaxiSavings = {
    this.copy(transactions)
  }

  override def interestEarned: Double = {

    // old interest calculation
//    sumTransactions() match {
//      case amount if (amount <= 1000) => amount * 0.02
//      case amount if (amount <= 2000) => 20 + (amount - 1000) * 0.05
//      case amount => 70 + (amount - 2000) * 0.1
//    }

    hasWithdrawals(10) match {
      case true => calculateInterest(transactions, 0.001)
      case _ => calculateInterest(transactions, 0.05)
    }
  }


  def title: String = "Maxi Savings Account"


  private def calculateInterest(trans: List[Transaction], rate: Double): Double = {
    trans match {
      case head :: tail => {
        val interest = head.daysFromNow * head.amount * rate / 365

        interest + calculateInterest(tail, rate)
      }
      case _ => 0d
    }
  }

  def hasWithdrawals(numDays: Int): Boolean = {
    transactions.exists( trans => trans.daysFromNow <= 10 && trans.amount < 0)
  }
}