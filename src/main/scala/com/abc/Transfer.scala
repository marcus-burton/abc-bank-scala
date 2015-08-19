package com.abc

/**
 * Created by shreya on 8/19/15.
 */

class Transfer(val fromAccount: Account, val toAccount: Account) {


  def makeTransfer(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    if (fromAccount == null || toAccount == null)
       throw new IllegalArgumentException(" Invalid Accounts")
    if (!fromAccount.customerName.trim.equalsIgnoreCase(toAccount.customerName.trim))
       throw new IllegalArgumentException(" The Accounts do not belong to the the same Customer")
    if (fromAccount.sumTransactions() < amount)
        throw new IllegalArgumentException("Cannot make transfer as insufficient funds in Account ")
    // Checks Complete - Make the Transfer
    fromAccount.withdraw(amount)
    toAccount.deposit(amount)
  }
}
