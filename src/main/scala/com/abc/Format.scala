package com.abc

trait Format {
  def toDollars(number: Double): String = f"$$$number%.2f"
}