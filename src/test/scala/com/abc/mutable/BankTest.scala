package com.abc.mutable

import com.abc.pure.{AccountType, EpochInstantProvider}
import org.scalatest.{Matchers, WordSpec}

class BankTest extends WordSpec with Matchers {

  implicit val timestamps = EpochInstantProvider

  "Bank.apply" should {
    "return banks with distinct memories" when {
      "called multiple times" in {
        val bankA, bankB = Bank()
        bankA.state.peek shouldNot be theSameInstanceAs bankB.state.peek
      }
    }
  }

  "addCustomer" should {
    "return a customer having the specified name" when {
      "called" in { Bank().addCustomer("Renée").name shouldBe "Renée" }
    }
  }

  "customers" should {
    "return empty" when {
      "the bank has no customers" in { Bank().customers shouldBe 'empty }
    }

    "return all customers previously added to the bank" when {
      "customers have been added" in {
        val bank = Bank()
        val names = Seq("Bob", "Ted", "Carol", "Alice", "Repeated", "Repeated")
        for (name <- names) bank.addCustomer(name)
        val customers = bank.customers
        customers.size shouldBe names.size
        customers.map(_.name) shouldBe names.toSet
      }
    }
  }

  "customerSummary" should {
    "return a summary" when {
      "called" in {
        Bank().customerSummary should startWith("Customer Summary")
      }
    }
  }

  "totalInterestPaid" should {
    "return zero" when {
      "there are no accounts" in { Bank().totalInterestPaid shouldBe 0 }
    }

    "return non-zero" when {
      "there is at least one non-empty account" in {
        val bank = Bank()
        bank
          .addCustomer("Jenny")
          .openAccount(AccountType.Checking)
          .deposit(8675309)
        bank.totalInterestPaid shouldNot be(0)
      }
    }
  }

  "clone" should {
    "share history but not updates" when {
      "the clone and the original object are manipulated independently" in {
        val bank1 = Bank()
        val acct1 = bank1.addCustomer("Alice").openAccount(AccountType.Checking)
        acct1.deposit(10000)
        val bank2 = bank1.clone
        acct1.withdraw(5000)
        bank1.totalInterestPaid shouldBe 5
        bank2.totalInterestPaid shouldBe 10
        bank2
          .addCustomer("Bob")
          .openAccount(AccountType.Checking)
          .deposit(20000)
        bank1.totalInterestPaid shouldBe 5
        bank2.totalInterestPaid shouldBe 30
      }
    }
  }
}
