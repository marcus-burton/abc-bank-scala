package com.abc.account

import com.abc.Account
import scala.collection.mutable.ListBuffer
import com.abc.AccountType
import com.abc.Transaction
import com.abc.TransactionType
import com.abc.util.DateProvider
import com.abc.util.Utils

class ExtendedMaxiSavingsAccount extends MaxiSavingsAccount {

  override def interestEarned(): Double = {
    val amount: Double = getBalance

    if (hasWithDrawInPast10Days()) {
			amount * 0.001
		}
		else {
			amount * 0.05
		}
  }

  private def hasWithDrawInPast10Days(): Boolean = {
		//10 days in milliseconds = 10 * 24 * 60 * 60 * 1000;
		val millisecondsOf10Days: Long = 864000000;
		var hasWithdraw = false;
		for (t <- this.getTransactions) {
			if (t.transactionType == TransactionType.WITHDRAWAL) {
				val timeOf10DaysAgo: Long  = DateProvider.now.getTime() - millisecondsOf10Days;
			
				if (t.getTransactionDate.getTime() >= timeOf10DaysAgo)
					hasWithdraw = true;
			}
		}
		
		return hasWithdraw;
	}
}
