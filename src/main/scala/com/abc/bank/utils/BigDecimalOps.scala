package com.abc.bank.utils

import scalaz.syntax.Ops

trait BigDecimalOps extends Ops[BigDecimal] {
  def toDollars: String = f"$$$self%.2f"
}


trait ToBigDecimalOps {
    implicit def toConfigOps(number: BigDecimal): BigDecimalOps = new BigDecimalOps {
      override def self: BigDecimal = number
    }
  }