package com.abc

import scala.collection.mutable.ListBuffer
import org.joda.time.{ DateTime, LocalDate }

case class Transaction(val amount: BigDecimal, val transactionDate: DateTime) {
  def toDateTransaction = DateTransaction(amount, transactionDate.toLocalDate)
}

/**
 * A transaction for a date. Hase no knowledge of the time
 */
case class DateTransaction(val amount: BigDecimal, val transactionDate: LocalDate)

object DateTransaction {
  private case class Accumulator[A](list: ListBuffer[A], nextElem: A)

  /**
   * Accumulate a list of Transactions into a list of DateTransaction where
   * each day is combined (amounts added together).
   *
   * NOTE: The list of assumed to be sorted by date
   */
  def fromTransactions(ts: ListBuffer[Transaction]): ListBuffer[DateTransaction] = {
    ts match {
      case l if l.length <= 1 => l.map(_.toDateTransaction)
      case l =>
        val accum = l.tail.foldLeft(Accumulator(ListBuffer.empty, l.head.toDateTransaction)) {
          case (Accumulator(acc, next), t) =>
            if (t.transactionDate.toLocalDate == next.transactionDate)
              Accumulator(acc, DateTransaction(next.amount + t.amount, next.transactionDate))
            else
              Accumulator(acc += next, t.toDateTransaction)
        }
        accum.list += accum.nextElem
    }
  }
}
