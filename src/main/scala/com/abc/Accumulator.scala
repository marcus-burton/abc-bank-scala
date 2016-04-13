package com.abc

import scala.collection.mutable.ListBuffer
import org.joda.time.{ Days, LocalDate }

/**
 * A DayCounter counts how many days are there between combined transactions.
 * If `DayDiff`, then we have a concrete count. Otherwise, with `UntilNext`,
 * we have an end, which means it should be compared to `right now` (today).
 */
sealed trait DayCounter
case class DayDiff(days: Long) extends DayCounter
case class UntilNext(from: LocalDate) extends DayCounter

object Accumulator {
  private case class Accumulator[A](list: ListBuffer[A], nextElem: A)

  /**
   * An AmountRange is simply a triplet of a BigDecimal (the amount at some
   * day), a DayCounter (either a flat number of days between this day and
   * the next transaction) and whether a withdrawal happened at this day.
   *
   * The `A <: DayCounter` is just for allowing for more typesafety when
   * creating these, it can be ignored for the most part.
   */
  type AmountRange[A <: DayCounter] = (BigDecimal, A, Boolean)

  /**
   * Accumulate all transactions into a walkable path of amounts and ranges,
   * keeping track of when a withdraw happens. 
   *
   * Axioms:
   * * The list is ordered by transactionDate
   * * The first transaction will never be negative
   * * The sum of all the transactions from the beginning to some point `n` on the
   *   list will never be negative.
   *
   * @return an order list of an amount (how much for a given day), a count
   *         (that states how many days are there between an amount and the
   *         next one) and whether a withdraw has happened in that day.
   *         The last element will contain an actual date instead of a count
   *         since it needs to be compared to "today".
   */
  def accumulate(list: ListBuffer[Transaction]): ListBuffer[AmountRange[DayCounter]] =
    list match {
      case l if l.length == 0 => ListBuffer.empty
      case l =>
        val base = Accumulator(ListBuffer.empty[AmountRange[DayCounter]], transToAmount(l.head))
        val lb = l.tail.foldLeft(base) {
          case (Accumulator(l, (amt, d@UntilNext(date), isW)), t) =>
            if (date == t.transactionDate.toLocalDate)
              Accumulator(l, (amt + t.amount, d, isW || t.amount < 0))
            else {
              val tAmt = transToAmount(t)
              val dayD = Days.daysBetween(date, tAmt._2.from).getDays.toLong
              Accumulator(l += ((amt, DayDiff(dayD), isW)), tAmt)
            }
          case _ =>
            throw new Exception("accumulate: Unreachable code")
        }
        lb.list += lb.nextElem
    }

  private def transToAmount(t: Transaction): AmountRange[UntilNext] =
    (t.amount, UntilNext(t.transactionDate.toLocalDate), t.amount < 0)
}
