package com.abc

import com.abc.TxType.TxType


/**
  * Created by ravi on 2/5/2017.
  */


case class Transaction (val amount:BigDecimal , TranType: TxType,
                        TimeStamp: String = getCurrentTime ) {
  def TxDetailsString = f"$TranType%-13s  Amount $amount%20.2f  TxDate $TimeStamp"
}