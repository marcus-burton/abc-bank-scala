package com.abc

import com.abc.transaction.{DepositTransaction, Transaction, TransferTransaction, WithdrawalTransaction}

class StatementPrinter(val accounts: List[Account], val customerName:String) {

  /**
    * This method gets a statement
    */
  def getStatement: String = {
    val totalAcrossAllAccounts = accounts.map(_.balance).sum
    val statement = f"Statement for $customerName\n" +
      accounts.map(statementForAccount).mkString("\n", "\n\n", "\n") +
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
      case _ => "Unknown Account Type\n"
    }

    val transactionSummary = a.transactions.map(t => transactionDescription(t) + " " + toDollars(t.amount.abs))
                             .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(a.findDepositAmount() - a.findWithdrawalAmount())}"

    accountType + transactionSummary + totalSummary
  }

  private def transactionDescription(t: Transaction) =
    t match {
      case WithdrawalTransaction(_, _) => "withdrawal"
      case DepositTransaction(_, _) => "deposit"
      case TransferTransaction(_, _, _) => "transfer"
      case _ => "N/A"
    }

  private def toDollars(number: Double): String = f"$$$number%.2f"
}
