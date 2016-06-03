package com.abc

import java.time.Instant

import org.scalatest.{Matchers, FlatSpec}

import collection.mutable.ListBuffer

class AccountTest extends FlatSpec with Matchers {
  "Account" should "report interests correctly for small amounts" in {
    val transactions = ListBuffer(
      Transaction(150.0, Instant.parse("2015-02-04T12:45:34Z")),
      Transaction(160.0, Instant.parse("2016-02-04T12:45:34Z"))
    )

    // Results compared with calculations done here:
    // http://www.thecalculatorsite.com/articles/finance/compound-interest-formula.php
    val checking = new Account(Account.Checking, transactions)
    round(checking.interestEarned(Instant.parse("2016-02-04T12:45:34Z"))) should be (0.15)
    round(checking.interestEarned(Instant.parse("2017-02-04T12:45:34Z"))) should be (0.46)

    val savings = new Account(Account.Savings, transactions)
    round(savings.interestEarned(Instant.parse("2016-02-04T12:45:34Z"))) should be (0.15)
    round(savings.interestEarned(Instant.parse("2017-02-04T12:45:34Z"))) should be (0.46)

    val maxi = new Account(Account.MaxiSavings, transactions)
    round(maxi.interestEarned(Instant.parse("2016-02-04T12:45:34Z"))) should be (7.5)
    round(maxi.interestEarned(Instant.parse("2017-02-04T12:45:34Z"))) should be (23.42)
  }

  it should "report interests correctly for larger amounts" in {
    val transactions = ListBuffer(
      Transaction(1000.0, Instant.parse("2011-02-04T12:40:00Z")),
      Transaction(1000.0, Instant.parse("2011-02-04T18:22:00Z")),
      Transaction(2000.0, Instant.parse("2012-02-04T12:45:00Z")),
      Transaction(-1000.0, Instant.parse("2013-02-04T12:45:00Z")),
      Transaction(10000.0, Instant.parse("2014-02-04T12:45:00Z")),
      Transaction(7000.0, Instant.parse("2015-02-04T12:45:00Z"))
    )

    // Results compared with calculations done here:
    // http://www.thecalculatorsite.com/articles/finance/compound-interest-formula.php
    val checking = new Account(Account.Checking, transactions)
    round(checking.interestEarned(Instant.parse("2012-02-04T00:00:00Z"))) should be (2.00)
    round(checking.interestEarned(Instant.parse("2016-02-04T00:00:00Z"))) should be (42.02)

    val savings = new Account(Account.Savings, transactions)
    round(savings.interestEarned(Instant.parse("2012-02-04T00:00:00Z"))) should be (2.99)
    round(savings.interestEarned(Instant.parse("2016-02-04T00:00:00Z"))) should be (79.13)

    val maxi = new Account(Account.MaxiSavings, transactions)
    round(maxi.interestEarned(Instant.parse("2012-02-04T00:00:00Z"))) should be (99.71)
    round(maxi.interestEarned(Instant.parse("2016-02-04T00:00:00Z"))) should be (2198.51)
  }

  def round(n: BigDecimal) = n.setScale(2, BigDecimal.RoundingMode.HALF_UP)
}
