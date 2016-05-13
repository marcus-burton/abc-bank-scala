package com.abc

package object Account{
trait Account {val owner: String}
case class Savings(val owner: String) extends Account
case class Checking(val owner: String) extends Account
case class MaxSavings(val owner: String) extends Account
}