package com.abc

import scala.collection.mutable.ListBuffer
import org.joda.time.{ DateTime, Days, Hours, LocalDate }

sealed trait Account {
  val id: String
  val transactions: ListBuffer[Transaction] = ListBuffer()

  /**
   * Defines how each Account deals with interests, including how
   * to punish them.
   * @return A function that, when given a date range, will return the
   *         amount with interest and how much more punishment is needed
   */
  protected def getWithInterest(amt: BigDecimal, punishDays: Long): Long => (BigDecimal, Long)

  def deposit(amount: Double): Unit =
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else {
      val now = DateTime.now
      transactions += Transaction(amount, now)
    }

  def withdraw(amount: Double): Unit =
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else {
      val now = DateTime.now
      transactions += Transaction(-amount, now)
    }

  /**
   * Transfer `amount` to `other`
   */
  def transferTo(amount: Double, other: Account): Unit = {
    this.withdraw(amount)
    other.deposit(amount)
  }

  /**
   * How much interest was earned by this account.
   */
  def interestEarned: BigDecimal = {
    val today = LocalDate.now
    val (withI, sum, _) = Accumulator.accumulate(transactions)
      .foldLeft((BigDecimal(0), BigDecimal(0), 0l)) {
        case ((withI, s, rePunish), (amt, days, isWithdraw)) =>
          val punish = if (isWithdraw) 10
                       else rePunish
          val getIntWithPunish = getWithInterest(withI + amt, punish)
          val (newP, morePunish) = days match {
            case DayDiff(d) => getIntWithPunish(d)
            case UntilNext(date) =>
              val d: Long = Days.daysBetween(date, today).getDays.toLong
              getIntWithPunish(d)
          }
          (newP, s + amt, morePunish)
      }
    withI - sum
  }

  def sumTransactions(): BigDecimal = transactions.foldLeft(BigDecimal(0))(_ + _.amount)


  // For tests
  private[abc] def addTransaction(t: Transaction): Unit =
    transactions += t
}

object Account {
  /**
   * Given a principal amount, a number of days and a yearly rate,
   * compute the compound interest with the principal amount.
   */
  def withCompoundInterest(pv: BigDecimal, numOfPeriods: Long, i: Double): BigDecimal = {
    val rateAtPeriod = i / 365
    pv * math.pow((1 + rateAtPeriod), numOfPeriods.toDouble)
  }

}

case class CheckingAccount(val id: String) extends Account {
  def getWithInterest(amt: BigDecimal, punish: Long) = {
    d => (Account.withCompoundInterest(amt, d, 0.001), 0l)
  }
}

case class SavingsAccount(val id: String) extends Account {
  def getWithInterest(amt: BigDecimal, punish: Long) = {
    val noPunish = 0l
    d => {
      val newP =
        if (amt <= 1000) Account.withCompoundInterest(amt, d, 0.001)
        else Account.withCompoundInterest(1000, d, 0.001) +
              Account.withCompoundInterest(amt - 1000, d, 0.002)
      (newP, noPunish)
    }
  }
}

case class MaxiSavingsAccount(val id: String) extends Account {
  def getWithInterest(amt: BigDecimal, punish: Long) = {
    d =>
      if (d > punish) { // Punish and forgive
        val int1 = Account.withCompoundInterest(amt, punish, 0.001)
        (Account.withCompoundInterest(int1, d - punish, 0.05), 0l)
      }
      else if (d < punish) // Punish and punish the next one too
        (Account.withCompoundInterest(amt, d, 0.001), punish - d)
      else // You have served your sentence
        (Account.withCompoundInterest(amt, d, 0.05), 0l)
  }
}
