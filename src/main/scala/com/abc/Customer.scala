package com.abc

case class Customer(name: String, accounts: Map[AccountType, Account] = Map.empty[AccountType, Account]) {

  def openAccount(account: Account): Customer =
    if (accounts.contains(account.accountType))
      throw new IllegalArgumentException(s"User already has an account of type ${account.accountType.name}")
    else
      copy(accounts = accounts + (account.accountType -> account))

  def numberOfAccounts: Int =
    accounts.size

  def totalInterestEarned: Double =
    accounts.values.map(_.interestEarned).sum

  def getStatement: String = {
    val totalAcrossAllAccounts = accounts.values.map(_.sumTransactions()).sum
    val totalsText = toDollars(totalAcrossAllAccounts)
    val statementsForEachAccount = accounts.values.map(statementForAccount).mkString("\n", "\n\n", "\n")
    s"Statement for $name\n$statementsForEachAccount\nTotal In All Accounts $totalsText"
  }

  private def statementForAccount(account: Account): String = {
    val accountType = account.accountType match {
      case CHECKING =>
        "Checking Account\n"
      case SAVINGS =>
        "Savings Account\n"
      case MAXI_SAVINGS =>
        "Maxi Savings Account\n"
    }
    val formatter = (t: Transaction) => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs)
    val transactionSummary = account.transactionSummary(formatter)
    val totalDollars = toDollars(account.sumTransactions())
    val totalSummary = s"Total ${totalDollars}"
    accountType + transactionSummary + totalSummary
  }

  private def withdrawalOrDepositText(t: Transaction) =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A" // this is a bit worrying since you've got a bigger problem!
    }

  private def toDollars(number: Double): String = f"$$$number%.2f"
}

