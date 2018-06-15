package com.abc

import java.time._
import java.time.temporal._

object DateProvider {
  def now: LocalDate = LocalDate.now
  def spec(yr: Int, mm: Int, dd: Int): LocalDate = LocalDate.of(yr,mm,dd)
  def daysFrom(from: LocalDate, to: LocalDate = LocalDate.now): Int = 
    from.until(to, ChronoUnit.DAYS).toInt
}

class DateProvider {
}

