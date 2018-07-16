package com.abc.mutable

import com.abc.pure.{AccountId, AccountType, EpochInstantProvider}
import com.abc.{memory, pure}
import org.scalatest.{EitherValues, Matchers, WordSpec}

class AccountTest extends WordSpec with Matchers with EitherValues {

  import AccountType.{Checking, Savings, MaxiSavings}

  implicit val timestamps = EpochInstantProvider

  def openAccount(typ: AccountType = Checking): Account = {
    val (bank, accountId) = {
      val (bank, john) = pure.Bank().addCustomer("John")
      bank.openAccount(john, typ).right.value
    }
    new Account(memory.Transactional(bank), accountId)
  }

  "wrapped" should {
    "throw" when {
      "the account ID is not recognized by the pure bank" in {
        val state = memory.Transactional(pure.Bank())
        val account = new Account(state, AccountId(0))
        assertThrows[Exception] { account.wrapped }
      }
    }

    "return the underlying account" when {
      "the account ID is recognized by the pure bank" in {
        openAccount(Checking).wrapped.accountType shouldBe Checking
      }
    }
  }

  "accountType" should {
    "return the type of the account" when {
      "the type is Checking" in {
        openAccount(Checking).accountType shouldBe Checking
      }

      "the type is Savings" in {
        openAccount(Savings).accountType shouldBe Savings
      }

      "the type is MaxiSavings" in {
        openAccount(MaxiSavings).accountType shouldBe MaxiSavings
      }
    }
  }

  "deposit" should {
    "throw" when {
      "the pure bank returns Left" in {
        val account = openAccount()
        assertThrows[Exception] { account.deposit(0) }
      }
    }

    "not throw" when {
      "the pure bank returns Right" in { openAccount().deposit(100) }
    }
  }

  "withdraw" should {
    "throw" when {
      "the pure bank returns Left" in {
        val account = openAccount()
        assertThrows[Exception] { account.withdraw(0) }
      }
    }
    "not throw" when {
      "the pure bank returns Right" in {
        val account = openAccount()
        account.deposit(100)
        account.withdraw(100)
      }
    }
  }

  "balance" should {
    "return the account balance" when {
      "the balance is zero" in { openAccount().balance shouldBe 0 }

      "the balance is non-zero" in {
        val account = openAccount()
        account.deposit(100)
        account.withdraw(68)
        account.deposit(10)
        account.balance shouldBe 42
      }
    }
  }

  "transactions" should {
    "return empty" when {
      "the account has no activity" in {
        openAccount().transactions shouldBe 'empty
      }
    }

    "return the account transactions" when {
      "the account has activity" in {
        val account = openAccount()
        account.deposit(100)
        account.withdraw(68)
        account.deposit(10)
        account.transactions.map(_.amount) shouldBe Seq(100, -68, 10)
      }
    }
  }

  "interestEarned" should {
    "return zero" when {
      "the account has a zero balance" in {
        openAccount().interestEarned shouldBe 0
      }
    }

    "return the interest from the wrapped account" when {
      "a checking account has a non-zero balance" in {
        val account = openAccount(Checking)
        account.deposit(10000)
        account.interestEarned shouldBe 10
      }

      "a savings account has a non-zero balance" in {
        val account = openAccount(Savings)
        account.deposit(10000)
        account.interestEarned shouldBe 19
      }

      "a maxi-savings account has a non-zero balance" in {
        val account = openAccount(MaxiSavings)
        account.deposit(10000)
        account.interestEarned shouldBe 870
      }
    }
  }
}
