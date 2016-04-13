package com.abc

import org.scalatest.{ Matchers, FlatSpec }
import Account.withCompoundInterest
import java.math.MathContext

class InterestTest extends FlatSpec with Matchers {

  def formatD(x: BigDecimal) = x.setScale(4, BigDecimal.RoundingMode.HALF_UP)

  "withCompoundInterest" should "return interest compounded over days" in {
    val rate = 0.001
    val days = 5l
    val p = 1000d
    val added = (0l until days).foldLeft(p) { (acc, _) =>
      acc + acc * (rate/365)
    }
    withCompoundInterest(p, days, rate) should be(added)
  }

  it should "compound over a year to yield over `p * rate`" in {
    val rate = 0.001
    val days = 365l
    val p = BigDecimal(1000)
    withCompoundInterest(p, days, rate) should be > (p * (rate + 1))
  }

  it should "adding the compound of variable rates add up" in {
    val rate1 = 0.001
    val rate2 = 0.002
    val dailyR1 = rate1 / 365
    val dailyR2 = rate2 / 365
    val days = 10l
    val p = BigDecimal(20000)
    val added = (0l until days).foldLeft(p) { (acc, _) =>
      val interest =
        if (acc <= 1000) acc * dailyR1
        else BigDecimal(1000) * dailyR1 + (acc - 1000) * dailyR2
      acc + interest
    }
    val sumOfComp = {
      val i = withCompoundInterest(1000, days, rate1)
      val x = withCompoundInterest(p - 1000, days, rate2)
      i + x
    }
    formatD(sumOfComp) should be (formatD(added))
  }
}
