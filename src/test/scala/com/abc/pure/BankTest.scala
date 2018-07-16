package com.abc.pure

import org.scalatest.{EitherValues, Matchers, WordSpec}

class BankTest extends WordSpec with Matchers with EitherValues {

  import AccountType.{Checking, Savings, MaxiSavings}

  implicit val forTimestamps = EpochInstantProvider

  def addCustomers(bank: Bank, names: Seq[String]): (Bank, Seq[CustomerId]) =
    names.foldLeft((Bank(), Seq.empty[CustomerId])) {
      case ((bank, ids), name) =>
        val (next, id) = bank.addCustomer(name)
        (next, ids :+ id)
    }

  "Bank.apply" should {
    "return a Bank" when {
      "called" in { assert(Bank().isInstanceOf[Bank]) }
    }
  }

  "addCustomer" should {
    "assign unique IDs" when {
      "called repeatedly" in {
        val (_, ids) = addCustomers(Bank(), Seq("Alice", "Bob", "Charlie"))
        ids.distinct.size shouldBe ids.size
      }
    }
  }

  "customers" should {
    "be empty" when {
      "the bank has no customers" in { Bank().customers shouldBe 'empty }
    }

    "be keyed by the IDs of all added customers" when {
      "the bank has customers" in {
        val (bank0, id0) = Bank().addCustomer("Customer 0")
        val (bank1, id1) = bank0.addCustomer("Customer 1")
        val (bank2, id2) = bank1.addCustomer("Customer 2")
        val (bank3, id3) = bank2.addCustomer("Customer 3")
        bank3.customers.size shouldBe 4
        bank3.customers.keys.toSet shouldBe Set(id0, id1, id2, id3)
      }
    }
  }

  "findCustomer" should {
    "return Left" when {
      "the customer ID is unrecognized" in {
        Bank().findCustomer(CustomerId(0)) shouldBe 'left
      }
    }
    "find the identified customer" when {
      "the customer ID is recognized" in {
        val names = (0 until 10).map(i => s"Customer #$i")
        val (bank, ids) = addCustomers(Bank(), names)
        for ((id, name) <- ids.zip(names))
          bank.findCustomer(id).map(_.name) shouldBe Right(name)
      }
    }
  }

  "openAccount" should {
    "assign unique account IDs" when {
      "called repeatedly" in {

        val (bank, Seq(alice, bob, charlie)) =
          addCustomers(Bank(), Seq("Alice", "Bob", "Charlie"))

        // The table below covers all pairings of new, just-used, and
        // previously used CustomerID and AccountType.  The sidebar identifies
        // these cases as follows: 0 = first use, 1 = back-to-back use, 2 =
        // past use.

        val (_, ids) = Seq( //      CustomerId  AccountType
          (alice, Checking), //         0          0
          (bob, Checking), //           0          1
          (bob, Savings), //            1          0
          (bob, Savings), //            1          1
          (charlie, Checking), //       0          2
          (charlie, Savings), //        1          2
          (alice, MaxiSavings), //      2          0
          (bob, MaxiSavings), //        2          1
          (charlie, Checking) //        2          2
        ).foldLeft((bank, Seq.empty[AccountId])) {
          case ((bank, ids), (cust, typ)) =>
            val (next, id) = bank.openAccount(cust, typ).right.value
            (next, ids :+ id)
        }
        ids.distinct.size shouldBe ids.size
      }
    }
  }

  "findAccount" should {
    "return Left" when {
      "the account ID is unrecognized" in {
        Bank().findAccount(AccountId(0)) shouldBe 'left
      }
    }
    "find the identified account" when {
      "the account ID is recognized" in {
        val (bank, accountIds) = {
          val (bank0, alice) = Bank().addCustomer("Alice")
          val (bank1, bob) = bank0.addCustomer("Bob")
          Seq(
            (alice, Checking, 1),
            (alice, Checking, 2),
            (alice, Savings, 3),
            (bob, Savings, 4)
          ).foldLeft((bank1, Vector.empty[AccountId])) {
            case ((bank, ids), (cust, typ, amount)) =>
              val (next, id) = (for {
                (bank, id) <- bank.openAccount(cust, typ).toOption
                bank <- bank.deposit(id, amount).toOption
              } yield (bank, id)).get
              (next, ids :+ id)
          }
        }
        val accounts = accountIds.map(id => bank.findAccount(id).right.value)
        accounts.map(_.balance) shouldBe Seq(1, 2, 3, 4)
      }
    }
  }

  "link" should {
    "return Right" when {
      "the customer is not yet on the account" in {
        val (bank, Seq(alice, bob)) = addCustomers(Bank(), Seq("Alice", "Bob"))
        bank.openAccount(alice, Checking).flatMap {
          case (bank, acct) =>
            bank.link(bob, acct)
        } shouldBe 'right
      }
    }

    "return Left" when {
      "the customer ID is unrecognized" in {
        val (bank, alice) = Bank().addCustomer("Alice")
        val nobody = CustomerId(999)
        bank.openAccount(alice, Checking).flatMap {
          case (bank, acct) =>
            bank.link(nobody, acct)
        } shouldBe 'left
      }

      "the customer is already on the account" in {
        val (bank, alice) = Bank().addCustomer("Alice")
        bank.openAccount(alice, Checking).flatMap {
          case (bank, acct) =>
            bank.link(alice, acct)
        } shouldBe 'left
      }
    }
  }

  "deposit" should {
    "return Left" when {
      "the account ID is unrecognized" in {
        Bank().deposit(AccountId(0), 100) shouldBe 'left
      }

      "the amount is invalid" in {
        val (bank0, alice) = Bank().addCustomer("Alice")
        val (bank1, acct) = bank0.openAccount(alice, Checking).right.value
        bank1.deposit(acct, 0) shouldBe 'left
        bank1.deposit(acct, -1) shouldBe 'left
      }
    }

    "add amount to balance" when {
      "amount is positive" in {
        val (bank, accountId) = {
          val (bank, alice) = Bank().addCustomer("Alice")
          bank.openAccount(alice, Checking).right.value
        }
        (for {
          bank <- bank.deposit(accountId, 42)
          acct <- bank.findAccount(accountId)
        } yield acct.balance) shouldBe Right(42)
      }
    }
  }

  "withdraw" should {
    "return Left" when {
      "the account ID is unrecognized" in {
        Bank().withdraw(AccountId(0), 100) shouldBe 'left
      }

      "the amount is invalid" in {
        val (bank0, alice) = Bank().addCustomer("Alice")
        val (bank1, acct) = bank0.openAccount(alice, Checking).right.value
        bank1.withdraw(acct, 0) shouldBe 'left
        bank1.withdraw(acct, -1) shouldBe 'left
        bank1.withdraw(acct, 1) shouldBe 'left // exceeds balance
      }
    }

    "subtract amount from balance" when {
      "amount is positive and does not exceed balance" in {
        val (bank0, alice) = Bank().addCustomer("Alice")
        val (bank1, accountId) = bank0.openAccount(alice, Checking).right.value
        val bank = bank1.deposit(accountId, 42).right.value
        (for {
          bank <- bank.withdraw(accountId, 1)
          acct <- bank.findAccount(accountId)
        } yield acct.balance) shouldBe Right(41)
        (for {
          bank <- bank.withdraw(accountId, 42)
          acct <- bank.findAccount(accountId)
        } yield acct.balance) shouldBe Right(0)
      }
    }
  }

  "customerSummary" should {
    "summarize customers" when {
      "absent any accounts" in {
        val summaries = Seq("Alice", "Bob", "Charlie")
          .scanLeft(Bank())((bank, name) => bank.addCustomer(name)._1)
          .map(_.customerSummary)
        summaries(0) shouldBe "Customer Summary\n\n"
        summaries(1) shouldBe "Customer Summary\n - Alice (0 accounts)\n"
        summaries(2) shouldBe s"""|Customer Summary
            | - Alice (0 accounts)
            | - Bob (0 accounts)
            |""".stripMargin
        summaries(3) shouldBe s"""|Customer Summary
            | - Alice (0 accounts)
            | - Bob (0 accounts)
            | - Charlie (0 accounts)
            |""".stripMargin
      }
    }
  }

  "totalInterestPaid" should {
    "return zero" when {
      "there are no accounts" in { Bank().totalInterestPaid shouldBe 0 }

      "return the sum of the interest earned by all open accounts" when {
        "there are accounts" in {
          val (bank, Seq(alice, bob)) =
            addCustomers(Bank(), Seq("Alice", "Bob"))
          Seq((alice, Checking),
              (alice, Checking),
              (alice, Savings),
              (bob, Savings),
              (bob, MaxiSavings))
            .foldLeft(bank) {
              case (bank, (cust, typ)) =>
                (for {
                  pair <- bank.openAccount(cust, typ)
                  (bank, accountId) = pair
                  bank <- bank.deposit(accountId, 10000)
                } yield bank).right.value
            }
            .totalInterestPaid shouldBe 10 + 10 + 19 + 19 + 870
        }
      }
    }
  }

  "getStatement" should {
    "return Left" when {
      "the customer ID is unrecognized" in {
        Bank().getStatement(CustomerId(0)) shouldBe 'left
      }
    }

    "return the specified customer's statement" when {
      "the customer has no accounts" in {
        val (bank, henry) = Bank().addCustomer("Henry")
        val statement = bank.getStatement(henry).right.value
        statement shouldBe """|Statement for Henry
                              |
                              |
                              |Total In All Accounts $0.00
                              |""".stripMargin
      }

      "accounts have no transactions" in {
        val (bank, henry, checking, savings) = {
          val (bank, henry) = Bank().addCustomer("Henry")
          for {
            (bank, checking) <- bank.openAccount(henry, Checking).toOption
            (bank, savings) <- bank.openAccount(henry, Savings).toOption
            (bank, _) <- bank.openAccount(henry, MaxiSavings).toOption
          } yield (bank, henry, checking, savings)
        }.get
        val statement = bank.getStatement(henry).right.value
        statement shouldBe """|Statement for Henry
                              |
                              |Checking Account
                              |Total $0.00
                              |
                              |Savings Account
                              |Total $0.00
                              |
                              |Maxi Savings Account
                              |Total $0.00
                              |
                              |Total In All Accounts $0.00
                              |""".stripMargin
      }

      "the customer has accounts" in {

        val (bank, henry, checking, savings) = {
          val (bank, henry) = Bank().addCustomer("Henry")
          for {
            (bank, checking) <- bank.openAccount(henry, Checking).toOption
            (bank, savings) <- bank.openAccount(henry, Savings).toOption
          } yield (bank, henry, checking, savings)
        }.get

        val statement = (for {
          bank <- bank.deposit(checking, 100)
          bank <- bank.deposit(savings, 4000)
          bank <- bank.withdraw(savings, 200)
          stmt <- bank.getStatement(henry)
        } yield stmt).right.value

        statement shouldBe """|Statement for Henry
                              |
                              |Checking Account
                              |  deposit $100.00
                              |Total $100.00
                              |
                              |Savings Account
                              |  deposit $4000.00
                              |  withdrawal $200.00
                              |Total $3800.00
                              |
                              |Total In All Accounts $3900.00
                              |""".stripMargin
      }
    }
  }

  "getTotalInterestEarned" should {
    "return Left" when {
      "the customer ID is unrecognized" in {
        Bank().getTotalInterestEarned(CustomerId(0)) shouldBe 'left
      }
    }

    "return zero" when {
      "the customer has no accounts" in {
        val (bank, henry) = Bank().addCustomer("Henry")
        bank.getTotalInterestEarned(henry) shouldBe Right(0)
      }
    }

    "sum the interest earned by all the customer's accounts" when {
      "the customer has accounts" in {
        val (bank,
             henry,
             checking1,
             checking2,
             savings1,
             savings2,
             maxi1,
             maxi2) = {
          val (bank, henry) = Bank().addCustomer("Henry")
          for {
            (bank, checking1) <- bank.openAccount(henry, Checking).toOption
            (bank, checking2) <- bank.openAccount(henry, Checking).toOption
            (bank, savings1) <- bank.openAccount(henry, Savings).toOption
            (bank, savings2) <- bank.openAccount(henry, Savings).toOption
            (bank, maxi1) <- bank.openAccount(henry, MaxiSavings).toOption
            (bank, maxi2) <- bank.openAccount(henry, MaxiSavings).toOption
          } yield
            (bank,
             henry,
             checking1,
             checking2,
             savings1,
             savings2,
             maxi1,
             maxi2)
        }.get
        (for {
          bank <- bank.deposit(checking1, 10000)
          bank <- bank.deposit(checking2, 10000)
          bank <- bank.deposit(savings1, 10000)
          bank <- bank.deposit(savings2, 10000)
          bank <- bank.deposit(maxi1, 10000)
          bank <- bank.deposit(maxi2, 10000)
          interest <- bank.getTotalInterestEarned(henry)
        } yield interest) shouldBe Right((10 + 19 + 870) * 2)
      }
    }
  }
}
