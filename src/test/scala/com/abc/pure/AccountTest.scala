package com.abc.pure

import org.scalatest.{Matchers, WordSpec}

class AccountTest extends WordSpec with Matchers {

  import AccountType.{Checking, Savings, MaxiSavings}

  implicit val forTimestamps = EpochInstantProvider

  val checking = Account(AccountType.Checking)
  val savings = Account(AccountType.Savings)
  val maxi = Account(AccountType.MaxiSavings)

  "Account.apply" should {
    "create an account of the specified type" when {
      "type is Checking" in { checking.accountType shouldBe Checking }
      "type is Savings" in { savings.accountType shouldBe Savings }
      "type is MaxiSavings" in { maxi.accountType shouldBe MaxiSavings }
    }
  }

  "deposit" should {
    "add amount to balance" when {
      "amount is positive" in {
        val amount = BigDecimal("1.23")
        (for {
          account <- checking.deposit(amount)
          account <- account.deposit(amount)
        } yield account.balance) shouldBe Right(amount * 2)
      }
    }

    "return Left" when {
      "amount is zero" in { checking.deposit(0) shouldBe 'left }
      "amount is negative" in { checking.deposit(-1) shouldBe 'left }
    }
  }

  "withdraw" should {
    "subtract amount from balance" when {
      "amount is positive and does not exceed balance" in {
        (for {
          account <- checking.deposit(100)
          account <- account.withdraw(BigDecimal("99.99"))
        } yield account.balance) shouldBe Right(BigDecimal("0.01"))
      }
    }

    "return Left" when {
      "amount is zero" in { checking.withdraw(0) shouldBe 'left }
      "amount is negative" in { checking.withdraw(-1) shouldBe 'left }
      "amount exceeds balance" in {
        checking.withdraw(1) shouldBe 'left
        checking.deposit(1).flatMap(_.withdraw(2)) shouldBe 'left
      }
    }
  }

  "balance" should {
    "compute sum all transactions" when {
      "account is empty" in { checking.balance shouldBe 0 }
      "account has transactions" in {
        (for {
          account <- checking.deposit(BigDecimal("8.67"))
          account <- account.withdraw(BigDecimal("5.30"))
          account <- account.deposit(9)
        } yield account.balance) shouldBe Right(BigDecimal("12.37"))
      }
    }
  }

  "interestEarned" should {

    def expect(account: Account, balance: BigDecimal, interest: String) {
      (for (account <- account.deposit(balance))
        yield account.interestEarned) shouldBe Right(BigDecimal(interest))
    }

    "compute zero interest" when {
      "balance is zero" in {
        checking.interestEarned shouldBe 0
        savings.interestEarned shouldBe 0
        maxi.interestEarned shouldBe 0
      }
    }

    "compute 0.1% interest" when {
      "checking balance < 1000" in { expect(checking, 999, "0.999") }
      "checking balance = 1000" in { expect(checking, 1000, "1") }
      "checking balance < 2000" in { expect(checking, 1999, "1.999") }
      "checking balance = 2000" in { expect(checking, 2000, "2") }
      "checking balance < 3000" in { expect(checking, 2999, "2.999") }
      "checking balance = 3000" in { expect(checking, 3000, "3") }
      "checking balance > 3000" in { expect(checking, 10000, "10") }
    }

    "compute 0.1% interest for the first $1,000 then 0.2%" when {
      "savings balance < 1000" in { expect(savings, 999, "0.999") }
      "savings balance = 1000" in { expect(savings, 1000, "1") }
      "savings balance < 2000" in { expect(savings, 1999, "2.998") }
      "savings balance = 2000" in { expect(savings, 2000, "3") }
      "savings balance < 3000" in { expect(savings, 2999, "4.998") }
      "savings balance = 3000" in { expect(savings, 3000, "5") }
      "savings balance > 3000" in { expect(savings, 10000, "19") }
    }

    "compute 2% interest for the first $1,000 then 5% for the next $,000 then 10%" when {
      "maxi-savings balance < 1000" in { expect(maxi, 999, "19.98") }
      "maxi-savings balance = 1000" in { expect(maxi, 1000, "20") }
      "maxi-savings balance < 2000" in { expect(maxi, 1999, "69.95") }
      "maxi-savings balance = 2000" in { expect(maxi, 2000, "70") }
      "maxi-savings balance < 3000" in { expect(maxi, 2999, "169.9") }
      "maxi-savings balance = 3000" in { expect(maxi, 3000, "170") }
      "maxi-savings balance > 3000" in { expect(maxi, 10000, "870") }
    }
  }
}
