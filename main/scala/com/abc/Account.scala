package com.abc

import java.time.LocalDate

import com.abc.transaction.{DepositTransaction, Transaction, TransferTransaction, WithdrawalTransaction}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Account {
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
}

class Account(val accountId: Int, val accountType: Int, val transactions: ListBuffer[Transaction] = ListBuffer()) {

  var balance: Double = 0.0

  // This could be dealt with by always looking at the transactions ... but as the account grows it will get slower.
  // Also should be scoped in a way to avoid access outside of transactions.
  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      balance += amount
  }

  def withdrawal(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      balance += -amount
  }

  def interestEarned: Double = {
    accountType match {
      case Account.SAVINGS if balance > 1000 =>
        1 + (balance - 1000) * 0.002
      case Account.MAXI_SAVINGS if findWithdrawals(10).isEmpty =>
        balance * 0.05
      case _ =>
        balance * 0.001
    }
  }

  def findWithdrawals(days: Int = -1): mutable.Seq[Transaction] = {
    val withdrawals: mutable.Seq[Transaction] = transactions.filter {
      case WithdrawalTransaction(a, _) if a.accountId == accountId => true
      case TransferTransaction(a, _, _) if a.accountId == accountId => true
      case _ => false
    }

    if (days != -1) {
      val cutoffDate = LocalDate.now().minusDays(days)
      withdrawals.filter(t => {
        t.transactionDate.isAfter(cutoffDate)
      })
    } else {
      withdrawals
    }
  }

  def findDeposits(days: Int = -1): mutable.Seq[Transaction] = {
    val deposits: mutable.Seq[Transaction] = transactions.filter {
      case DepositTransaction(a, _) if a.accountId == accountId => true
      case TransferTransaction(_, a, _) if a.accountId == accountId => true
      case _ => false
    }

    if (days != -1) {
      val cutoffDate = LocalDate.now().minusDays(days)
      deposits.filter(t => !t.transactionDate.isAfter(cutoffDate))
    } else {
      deposits
    }
  }

  def findWithdrawalAmount(): Double = {
    findWithdrawals().map(t => t.amount).sum
  }

  def findDepositAmount(): Double = {
    findDeposits().map(t => t.amount).sum
  }


}