package com.abc.mutable

import com.abc.{pure, memory}
import com.abc.pure.{AccountType, CustomerId, EpochInstantProvider}
import org.scalatest.{Matchers, WordSpec}

class CustomerTest extends WordSpec with Matchers {

  import AccountType.{Checking, Savings, MaxiSavings}

  implicit val timestamps = EpochInstantProvider

  lazy val badCustomer =
    new Customer(memory.Transactional(pure.Bank()), CustomerId(0))

  def newCustomer(name: String = "A. Customer"): Customer = {
    val (bank, customerId) = pure.Bank().addCustomer(name)
    new Customer(memory.Transactional(bank), customerId)
  }

  "wrapped" should {
    "throw" when {
      "the customer ID is not recognized by the pure bank" in {
        assertThrows[Exception] { badCustomer.wrapped }
      }
    }

    "return the underlying customer" when {
      "the customer ID is recognized by the pure bank" in {
        newCustomer("John").wrapped.name shouldBe "John"
      }
    }
  }

  "name" should {
    "return the customer name" when {
      "called" in { newCustomer("John").name shouldBe "John" }
    }
  }

  "openAccount" should {
    "throw" when {
      "the pure bank returns Left" in {
        assertThrows[Exception] { badCustomer.openAccount(Checking) }
      }
    }

    "return a new Account" when {
      "the pure bank returns Right" in {
        newCustomer().openAccount(Checking).balance shouldBe 0
      }
    }
  }

  "numberOfAccounts" should {
    "return the correct number of accounts" when {
      "the customer has no accounts" in {
        newCustomer().numberOfAccounts shouldBe 0
      }

      "the customer has accounts" in {
        val alice = newCustomer("Alice")
        alice.openAccount(Checking)
        alice.openAccount(Checking)
        alice.openAccount(Savings)
        alice.numberOfAccounts shouldBe 3
      }
    }
  }

  "accounts" should {
    "return a distinct object for each account" when {
      "the customer has no accounts" in {
        newCustomer().accounts shouldBe 'empty
      }

      "the customer has accounts" in {
        val alice = newCustomer("Alice")
        alice.openAccount(Checking)
        alice.openAccount(Checking)
        alice.openAccount(Savings)
        alice.openAccount(MaxiSavings)
        alice.openAccount(MaxiSavings)
        alice.accounts.groupBy(_.accountType).mapValues(_.size) shouldBe Map(
          Checking -> 2,
          Savings -> 1,
          MaxiSavings -> 2)
      }
    }
  }

  "totalInterestEarned" should {
    "throw" when {
      "the pure bank returns Left" in {
        assertThrows[Exception] { badCustomer.totalInterestEarned }
      }
    }

    "return the amount of interest" when {
      "the pure bank returns Right" in {
        val bob = newCustomer("Bob")
        bob.openAccount(Checking).deposit(5000)
        bob.totalInterestEarned shouldBe 5
      }
    }
  }

  "getStatement" should {
    "throw" when {
      "the pure bank returns Left" in {
        assertThrows[Exception] { badCustomer.getStatement }
      }
    }

    "return the customer's statement" when {
      "the pure bank returns Right" in {
        val (bob, joe) = (newCustomer("Bob"), newCustomer("Joe"))
        bob.getStatement should startWith("Statement for Bob")
        joe.getStatement should startWith("Statement for Joe")
      }
    }
  }
}
