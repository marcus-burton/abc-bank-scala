package com.abc.account

import com.abc.Account
import scala.collection.mutable.ListBuffer
import com.abc.AccountType
import com.abc.Transaction

class MaxiSavingsAccount extends Account {
  val accountType = AccountType.MAXI_SAVINGS;
  
  def interestEarned(): Double = {
    val amount: Double = getBalance
    
    if (amount <= 1000) return amount * 0.02
    
    if (amount <= 2000) return 20 + (amount - 1000) * 0.05
    
    70 + (amount - 2000) * 0.1
  }

}
