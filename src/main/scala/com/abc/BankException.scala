package com.abc

object BankExceptionType extends Enumeration {
  val BANK_EXCEPTION, ACCOUNT_EXCEPTION, CUSTOMER_EXCEPTION = Value
}

//only Exception class thrown by all objects at runtime
case class BankException(message: String, exceptionType: BankExceptionType.Value) extends Exception(message) {

}