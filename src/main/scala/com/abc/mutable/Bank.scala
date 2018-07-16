package com.abc.mutable

import com.abc.memory.Transactional
import com.abc.pure
import com.abc.pure.InstantProvider

object Bank {
  def apply()(implicit timestamps: InstantProvider = SystemInstantProvider) =
    new Bank(Transactional(pure.Bank()))
}

final class Bank private (private[mutable] val state: Transactional[pure.Bank])(
    implicit timestamps: InstantProvider) {

  import state.{commit, peek}

  def addCustomer(name: String): Customer = commit { bank =>
    val (next, customerId) = bank.addCustomer(name)
    (next, new Customer(state, customerId))
  }

  def customers: Set[Customer] =
    peek.customers.keys.map(id => new Customer(state, id)).toSet

  def customerSummary: String = peek.customerSummary

  def totalInterestPaid: BigDecimal = peek.totalInterestPaid

  override def clone(): Bank = new Bank(Transactional(peek))
}
