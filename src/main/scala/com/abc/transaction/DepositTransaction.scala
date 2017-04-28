package com.abc.transaction

import com.abc.Account

case class DepositTransaction(account: Account, amount: Double) extends AccountTransaction(account) {
  def process(): Unit = account.deposit(amount)
}

