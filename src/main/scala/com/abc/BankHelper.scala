package com.abc

/**
  * Created by dongyangzhao on 6/1/17.
  */
object BankHelper {
  def format(number: Int, word: String): String = number + " " + (if (number == 1) word else word + "s")
  def formatSummary(customer: Customer) =  "\n - " + customer.name + " (" + format(customer.numberOfAccounts, "account") + ")"
  def dayDiff(d1: java.util.Date, d2: java.util.Date): Int = Math.abs(((d2.getTime-d1.getTime)/(1000 * 60 * 60 * 24)).toInt)

}
