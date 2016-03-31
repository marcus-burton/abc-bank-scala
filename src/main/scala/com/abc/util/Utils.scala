package com.abc.util

object Utils {  
	
  def isBlankString(input : Option[String]) : Boolean = input match {
    case None    => true
    case Some(s) => s.trim.isEmpty
  }
  
	def isNull(input : Option[AnyRef]) : Boolean = input match {
	  case None => true
	  case _    => false
	}
	
	def toDollars(number: Double): String = f"$$$number%.2f"

}