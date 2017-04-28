package com.abc.transaction

import com.abc.Account

abstract class AccountTransaction(account: Account) extends Transaction {
  account.transactions += this
}
