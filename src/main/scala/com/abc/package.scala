package com

package object abc {
  type Transactions = List[Customer#Account#Transaction]
  type Customers = List[Customer]
  type Accounts = List[Customer#Account]
}
