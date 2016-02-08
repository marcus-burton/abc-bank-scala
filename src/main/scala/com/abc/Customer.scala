package com.abc

import scala.collection.mutable.ListBuffer

case class TransferFundsException(msg:String) extends RuntimeException

class Customer(val name: String, val accounts: ListBuffer[Account] = ListBuffer()) {

  def openAccount(account: Account): Customer = accounts.synchronized{
      accounts += account
      this
  }

  def numberOfAccounts: Int = accounts.synchronized {accounts.size}

  def totalInterestEarned: BigDecimal = accounts.synchronized{accounts.map(_.interestEarned).sum}

  def transferFunds(from: Account, to:Account, amount: BigDecimal):Unit = accounts.synchronized {
    if (from == to) throw new TransferFundsException("Can't transact within the same account")
    if (!(accounts contains from) || !(accounts contains to)) throw new TransferFundsException("Both accounts should be owned by the same customer")

    if (from.id < to.id){
      from.synchronized { to.synchronized{ adjust() }}
    }else{
      to.synchronized{ from.synchronized{ adjust() }}
    }

    def adjust(): Unit ={
      if (from.sumTransactions < amount) throw new InsufficientFundsException(s"Not enough funds at ${from}. Aborting transfer")
      from.withdraw(amount)
      to.deposit(amount)
    }
  }

  /**
   * This method gets a statement
   */
  def getStatement: String = accounts.synchronized {
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions).sum
    val statement = f"Statement for $name\n" +
      accounts.sortWith(_.accountType < _.accountType).map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
    statement
  }

  private def statementForAccount(a: Account): String = {
    val accountType = a.accountType match {
      case Account.CHECKING =>
        "Checking Account\n"
      case Account.SAVINGS =>
        "Savings Account\n"
      case Account.MAXI_SAVINGS =>
        "Maxi Savings Account\n"
    }
    val transactionSummary = a.transactionAmounts.map(a => withdrawalOrDepositText(a) + " " + toDollars(a.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(a.sumTransactions)}"
    accountType + transactionSummary + totalSummary
  }

  private def withdrawalOrDepositText(a: BigDecimal) =
    a match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

  private def toDollars(number: BigDecimal): String = f"$$$number%.2f"
}
