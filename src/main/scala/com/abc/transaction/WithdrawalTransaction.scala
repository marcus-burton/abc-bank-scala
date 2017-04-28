package com.abc.transaction

import com.abc.Account

case class WithdrawalTransaction(account: Account, amount: Double) extends AccountTransaction(account) {
  def process(): Unit = account.withdrawal(amount)
}

