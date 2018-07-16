package com.abc.mutable

import com.abc.memory.Transactional
import com.abc.pure
import com.abc.pure.CustomerId

final class Customer private[mutable] (state: Transactional[pure.Bank],
                                       customerId: CustomerId) {

  import state.{commit, peek}

  private[mutable] def wrapped = peek.findCustomer(customerId) match {
    case Left(message)   => throw new Exception(message)
    case Right(customer) => customer
  }

  def name: String = wrapped.name

  def openAccount(accountType: pure.AccountType): Account = commit { bank =>
    bank.openAccount(customerId, accountType) match {
      case Left(message)            => throw new Exception(message)
      case Right((bank, accountId)) => (bank, new Account(state, accountId))
    }
  }

  def numberOfAccounts: Int = wrapped.numberOfAccounts

  def accounts: Set[Account] = wrapped.accountIds.map(new Account(state, _))

  def totalInterestEarned: BigDecimal =
    peek.getTotalInterestEarned(customerId) match {
      case Left(message)   => throw new Exception(message)
      case Right(interest) => interest
    }

  def getStatement(): String = peek.getStatement(customerId) match {
    case Left(message)    => throw new Exception(message)
    case Right(statement) => statement
  }
}
