package com.abc.bank.model

import java.util.{Calendar, Date}


case class Transaction(
                        amount: BigDecimal,
                        transactionDate: Date = Calendar.getInstance.getTime
                      )