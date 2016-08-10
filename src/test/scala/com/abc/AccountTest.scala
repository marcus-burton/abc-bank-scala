package com.abc

import org.scalatest.{Matchers, FlatSpec}

class AccountTest extends FlatSpec with Matchers {
  it should "have account type value 0 for checking" in {
    val checkingAccount: Account = new Account(Account.CHECKING)
    checkingAccount.accountType should be(0)
  }

  it should "have account type value 1 for savings" in {
    val checkingAccount: Account = new Account(Account.SAVINGS)
    checkingAccount.accountType should be(1)
  }

  it should "have account type value 2 for maxisavings" in {
    val checkingAccount: Account = new Account(Account.MAXI_SAVINGS)
    checkingAccount.accountType should be(2)
  }
}
