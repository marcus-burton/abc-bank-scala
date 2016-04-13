package com.abc

import org.scalatest.{Matchers, FlatSpec}
import org.joda.time.DateTime
import java.math.MathContext
import collection.mutable.ListBuffer

class AccumulatorTest extends FlatSpec with Matchers {

  def now = DateTime.now

  "Accumulator" should "handle a single transaction fine" in {
    val now = this.now
    val r = Accumulator.accumulate(ListBuffer(Transaction(100, now)))
    r should not be empty
    r.head should be((100, UntilNext(now.toLocalDate), false))
  }

  it should "have the last, and only the last one, be the `UntilNext`" in {
    val now = this.now
    val r = Accumulator.accumulate(
      ListBuffer(
          Transaction(100, now.minusDays(3))
        , Transaction(250, now.minusDays(2))
        , Transaction(200, now.minusDays(1))
        , Transaction(-50, now)
        )
    )
    r should not be empty
    r.init.foreach { t =>
      t._2 should not be an [UntilNext]
    }
    r.last._2 shouldBe an [UntilNext]
  }

  it should "accumulate same dates" in {
    val now = this.now
    val r = Accumulator.accumulate(
      ListBuffer(
          Transaction(300, now.minusDays(3))
        , Transaction(100, now.minusDays(2))
        , Transaction(250, now.minusDays(2))
        , Transaction(200, now.minusDays(1))
        , Transaction(-50, now)
        )
    )
    r should not be empty
    r should have length 4
    r(1) should be((350, DayDiff(1), false))
  }

  it should "mark as `isWithdraw` if at least one transaction in a day is a withdraw" in {
    val now = this.now
    val r = Accumulator.accumulate(
      ListBuffer(
          Transaction(300, now.minusDays(3))
        , Transaction(100, now.minusDays(2))
        , Transaction(-10, now.minusDays(2))
        , Transaction(250, now.minusDays(2))
        , Transaction(200, now.minusDays(1))
        , Transaction(-50, now)
        )
    )
    r should not be empty
    r should have length 4
    r(1)._3 should be(true)
  }
}
