package com.abc.pure

import org.scalatest.{EitherValues, Matchers, WordSpec}

class CustomerTest extends WordSpec with Matchers with EitherValues {

  "Customer.apply" should {
    "return a customer having the specified name" when {
      def attempt(name: String) { Customer(name).name shouldBe name }
      "empty" in { attempt("") }
      "an English word" in { attempt("Pearl") }
      "hyphenated" in { attempt("William Gockel-Figge") }
      "non-ASCII" in { attempt("François Q. שלום-עליכם") }
    }
  }

  "addAccount" should {
    "return Right" when {
      "the ID is unique" in {
        val right = Right(Customer("Jenny"))
        (Seq(8, 6, 7, 5, 3, 0, 9)
          .map(AccountId.apply)
          .foldLeft(right: Either[String, Customer]) { (either, id) =>
            either.flatMap(_.addAccount(id))
          }) shouldBe 'right
      }
    }

    "return Left" when {
      "the ID is not unique" in {
        val right = Right(Customer("Pi"))
        (Seq(3, 1, 4, 1)
          .map(AccountId.apply)
          .foldLeft(right: Either[String, Customer]) { (either, id) =>
            either.flatMap(_.addAccount(id))
          }) shouldBe 'left
      }
    }
  }

  "numberOfAccounts" should {
    "return the number of accounts" when {
      "the customer has no accounts" in {
        Customer("").numberOfAccounts shouldBe 0
      }

      "the customer has an account" in {
        val id = AccountId(42)
        val douglas = Customer("Douglas").addAccount(id).right.value
        douglas.numberOfAccounts shouldBe 1
      }

      "the customer has multiple accounts" in {
        val ids = for (id <- Seq(8, 6, 7, 5, 3, 0, 9)) yield AccountId(id)
        val jenny = ids.foldLeft(Customer("jenny")) { (jenny, id) =>
          jenny.addAccount(id).right.value
        }
        jenny.numberOfAccounts shouldBe ids.size
      }
    }
  }
}
