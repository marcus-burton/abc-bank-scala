package com.abc

import java.util.UUID


// Moved all the functionality on "Bank" class
class Customer(val name: String) extends Ordered[Customer]{
  private val id = UUID.randomUUID().toString
  override def compare(that: Customer): Int = id.compare(that.id)

  def getId: String = id
}

