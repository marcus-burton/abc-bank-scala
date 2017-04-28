package com.abc.transaction

import com.abc.Account

case class TransferTransaction(fromAccount: Account, toAccount: Account, amount: Double) extends Transaction {

  fromAccount.transactions += this
  toAccount.transactions += this

  def process(): Unit = {

    fromAccount.withdrawal(amount)

    // poor man's transaction handling
    try {
      toAccount.deposit(amount)
    } catch {
      case _: IllegalArgumentException => {
        println("Failed to process deposit")
        fromAccount.deposit(amount)
      }
    }
  }
}
