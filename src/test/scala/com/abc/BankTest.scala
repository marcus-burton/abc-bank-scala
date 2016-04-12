package com.abc

import org.scalatest.{Matchers, FlatSpec}
import org.joda.time.DateTime
import Account.withCompoundInterest
import java.math.MathContext

class BankTest extends FlatSpec with Matchers {

  def now = DateTime.now()
  def depositAt(act: Account, amt: Double, time: DateTime) =
    act.addTransaction(Transaction(amt, time))

  /**
   * Utility for manual testing. Pass a sequence of triplets where
   * the first element is the account change, the second is the number of
   * days between transactions and the last is the expected rate for that
   * period.
   */
  def testCompound(changes: (BigDecimal, Int, Double)*) = {
    val (wI, wo) = changes.foldLeft(BigDecimal(0), BigDecimal(0)) {
      case ((acc, sum), (a, d, r)) =>
        (withCompoundInterest(acc + a, d, r), sum + a)
    }
    wI - wo
  }

  "Bank" should "customer summary" in {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John").openAccount(CheckingAccount("c1"))
    bank.addCustomer(john)
    bank.customerSummary should be("Customer Summary\n - John (1 account)")
  }

  "CheckingAccount" should "work with a single deposit" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = CheckingAccount("c1")
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    depositAt(checkingAccount, 100.0, now.minusDays(365))
    val bigDRes = BigDecimal(("%.2f").format(bank.totalInterestPaid))
    bigDRes should be(BigDecimal(0.1))
  }

  it should "work with multiple deposits spread throughout the year" in {
    val bank: Bank = new Bank
    val checkingAccount: Account = CheckingAccount("c1")
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    val time = now
    val rate = 0.001
    depositAt(checkingAccount, 100.0, time.minusDays(365))
    depositAt(checkingAccount, 200.0, time.minusDays(300))
    depositAt(checkingAccount, -200.0, time.minusDays(250))
    depositAt(checkingAccount, 200.0, time.minusDays(100))
    depositAt(checkingAccount, -50, time.minusDays(30))

    val testR = testCompound (
          (100, 65, rate)
        , (200, 50, rate)
        , (-200, 150, rate)
        , (200, 70, rate)
        , (-50, 30, rate)
    )

    bank.totalInterestPaid should be(testR)
  }

  "SavingsAccount" should "work with a single deposit one year ago" in {
    val bank: Bank = new Bank
    val savingsAccount: Account = SavingsAccount("s1")
    bank.addCustomer(new Customer("Bill").openAccount(savingsAccount))
    depositAt(savingsAccount, 1500.0, now.minusDays(365))

    val testR =
      Account.withCompoundInterest(1000, 365, 0.001) +
        Account.withCompoundInterest(500, 365, 0.002) - 1500

    bank.totalInterestPaid should be(testR)
  }

  it should "work with multiple deposits in a single day (5 days ago, 5 day compound)" in {
    val bank: Bank = new Bank
    val savingsAccount: Account = SavingsAccount("s1")
    bank.addCustomer(new Customer("Bill").openAccount(savingsAccount))
    val time = now.minusDays(5)
    depositAt(savingsAccount, 500.0, time)
    depositAt(savingsAccount, 1500.0, time)

    val testR = {
      Account.withCompoundInterest(1000, 5, 0.001) +
        Account.withCompoundInterest(1000, 5, 0.002) -
          2000
    }

    bank.totalInterestPaid should be(testR)
  }

  "MaxiSavingsAccount" should "with no withdrawals, have 5% (one year)" in {
    val bank: Bank = new Bank
    val mxAccount: Account = MaxiSavingsAccount("ms1")
    bank.addCustomer(new Customer("Bill").openAccount(mxAccount))

    depositAt(mxAccount, 1000, now.minusDays(365))
    depositAt(mxAccount, 3000, now.minusDays(365))

    val testR = testCompound(
        (4000, 365, 0.05)
      )

    bank.totalInterestPaid should be(testR)
  }

  it should "with withdrawals, have 0.1% for those 10 days. Then 5% afterwards" in {
    val bank: Bank = new Bank
    val mxAccount: Account = MaxiSavingsAccount("ms1")
    bank.addCustomer(new Customer("Bill").openAccount(mxAccount))

    val now = this.now
    depositAt(mxAccount, 3100, now.minusDays(365)) // from here until next transaction, accrue at 5%
    depositAt(mxAccount, -100, now.minusDays(15)) // from here until NOW, accrue at 0.1%

    val testR = testCompound(
        (3100, 350, 0.05)
      , (-100, 10, 0.001)
      , (0, 5, 0.05)
      )

    bank.totalInterestPaid should be(testR)
  }

  it should "with withdrawals, have 0.1% for 10 days even if a deposit happens a day later" in {
    val bank: Bank = new Bank
    val mxAccount: Account = MaxiSavingsAccount("ms1")
    bank.addCustomer(new Customer("Bill").openAccount(mxAccount))

    val now = this.now

    depositAt(mxAccount, 200, now.minusDays(20))
    depositAt(mxAccount, -100, now.minusDays(15))
    depositAt(mxAccount, 5, now.minusDays(14))

    val testR = testCompound(
        (200, 5, 0.05)
      , (-100, 1, 0.001)
      , (5, 9, 0.001)
      , (0, 5, 0.05)
      )

    bank.totalInterestPaid should be(testR)
  }

  it should "with withdrawals, have 0.1% for 10 days and, if another withdraw happens, refresh the punish from that point" in {
    val bank: Bank = new Bank
    val mxAccount: Account = MaxiSavingsAccount("ms1")
    bank.addCustomer(new Customer("Bill").openAccount(mxAccount))

    val now = this.now

    depositAt(mxAccount, 200, now.minusDays(20))
    depositAt(mxAccount, -100, now.minusDays(15))
    depositAt(mxAccount, 50, now.minusDays(14))
    depositAt(mxAccount, -50, now.minusDays(11))

    val testR = testCompound(
        (200, 5, 0.05)
      , (-100, 1, 0.001)
      , (50, 3, 0.001)
      , (-50, 10, 0.001)
      , (0, 1, 0.05)
      )

    bank.totalInterestPaid should be(testR)
  }

}
