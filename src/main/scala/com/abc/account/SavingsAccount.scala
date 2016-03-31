package com.abc.account

import com.abc.Account
import scala.collection.mutable.ListBuffer
import com.abc.AccountType
import com.abc.Transaction

class SavingsAccount extends Account {
  val accountType = AccountType.SAVINGS;

  def interestEarned(): Double = {
    val amount: Double = getBalance
    if (amount <= 1000) amount * 0.001
    else 1 + (amount - 1000) * 0.002
  }
  
}
