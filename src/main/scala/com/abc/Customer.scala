package com.abc

// could make accounts a concurrent Hashmap, immutable data structures are nice though  :)
case class Customer(name: String, accounts: Set[Account] = Set.empty[Account]) {

  def openAccount(account: Account): Customer =
    if (accounts.contains(account))
      throw new IllegalArgumentException(s"User already has an account of type ${account.name}")
    else
      copy(accounts = accounts + account)

  def numberOfAccounts: Int =
    accounts.size

  def totalInterestEarned: Double =
    accounts.map(_.interestEarned).sum

  def getStatement: String = {
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions()).sum
    val totalsText = toDollars(totalAcrossAllAccounts)
    val statementsForEachAccount = accounts.map(statementForAccount).mkString("\n", "\n\n", "\n")
    s"Statement for $name\n$statementsForEachAccount\nTotal In All Accounts $totalsText"
  }

  private def statementForAccount(account: Account): String = {
    val accountType = account match {
      case Checking() =>
        "Checking Account\n"
      case Savings() =>
        "Savings Account\n"
      case MaxiSavings() =>
        "Maxi Savings Account\n"
    }
    val formatter = (t: Transaction) => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs)
    val transactionSummary = account.transactionSummary(formatter)
    val totalDollars = toDollars(account.sumTransactions())
    val totalSummary = s"Total ${totalDollars}"
    s"$accountType$transactionSummary$totalSummary"
  }

  private def withdrawalOrDepositText(t: Transaction): String =
    if (t.amount < 0)
      "withdrawal"
    else if (t.amount > 0)
      "deposit"
    else
      "N/A" // this is a bit worrying since you've got a bigger problem!

  private def toDollars(number: Double): String = f"$$$number%.2f"
}

