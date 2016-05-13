package com.abc


case class Customer(val name: String, val accounts: Set[Account] = Set.empty) {

  def openAccount(account: Account): Customer = {
    this.copy(accounts = accounts + account)
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double = accounts.map(_.interestEarned).sum


  /**
    * Transfers money from two accounts
    * Provides some very basic validations
    */
  def transfer(from: Account, to: Account, amount: Double): Customer = {
    require(accounts.contains(from), "Account to transfer from must belong to the customer")
    require(accounts.contains(to), "Account to transfer to must belong to the customer")
    require(from.sumTransactions() > amount, "Not enough funds to transfer")

    val otherAccounts = accounts - (from, to)
    val fromNew = from.withdraw(amount)
    val toNew = to.deposit(amount)
    this.copy(accounts = otherAccounts + (fromNew, toNew))
  }

  /**
   * This method gets a statement
   */
  def getStatement: String = {
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions()).sum
    val statement = f"Statement for $name\n" +
      accounts.map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
    statement
  }


  def printSummary: String = {
    name + " (" + format(numberOfAccounts, "account") + ")"
  }



  private def statementForAccount(acct: Account): String = {
    val transactionSummary = acct.transactions.map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(acct.transactions.map(_.amount).sum)}"
    acct.title + "\n" + transactionSummary + totalSummary
  }

  private def withdrawalOrDepositText(t: Transaction) =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

  private def toDollars(number: Double): String = f"$$$number%.2f"

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }
}

