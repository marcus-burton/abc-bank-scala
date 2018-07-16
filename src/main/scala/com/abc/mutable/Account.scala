package com.abc.mutable

import com.abc.memory.Transactional
import com.abc.pure
import com.abc.pure.{AccountId, AccountType, Transaction}

final class Account private[mutable] (state: Transactional[pure.Bank],
                                      accountId: AccountId) {

  import state.{commit, peek}

  private[mutable] def wrapped = peek.findAccount(accountId) match {
    case Left(message)  => throw new Exception(message)
    case Right(account) => account
  }

  def accountType: AccountType = wrapped.accountType

  def balance: BigDecimal = wrapped.balance

  def deposit(amount: BigDecimal): Unit = state.commit { bank =>
    bank.deposit(accountId, amount) match {
      case Left(message) => throw new Exception(message)
      case Right(bank)   => (bank, ())
    }
  }

  def withdraw(amount: BigDecimal): Unit = state.commit { bank =>
    bank.withdraw(accountId, amount) match {
      case Left(message) => throw new Exception(message)
      case Right(bank)   => (bank, ())
    }
  }

  def transactions: Vector[Transaction] = wrapped.transactions

  def interestEarned: BigDecimal = wrapped.interestEarned
}
