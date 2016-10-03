package com.abc

import scala.collection.mutable.ListBuffer

class Customer(val name: String, var accounts: ListBuffer[Account] = ListBuffer()) {

  def openAccount(account: Account): Customer = {
    accounts += account
    this
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double = accounts.map(_.interestEarned).sum
  private var fromAccount = None: Option[Account]
  private var toAccount = None: Option[Account]


  /**
   * This method gets a statement
   */
  def getStatement: String = {
    //JIRA-123 Change by Joe Bloggs 29/7/1988 start
    var statement: String = null //reset statement to null here
    //JIRA-123 Change by Joe Bloggs 29/7/1988 end
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions()).sum
    statement = f"Statement for $name\n" +
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
    }
    val transactionSummary = a.transactions.map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(a.transactions.map(_.amount).sum)}"
    accountType + transactionSummary + totalSummary
  }

  private def withdrawalOrDepositText(t: Transaction) =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

 private def toDollars(number: Double): String = f"$$$number%.2f"
    
  // Adding method to find is account exist for Customer to and from transfer is happening
  private def accountExist(account: Account): Boolean = {
    accounts.contains(account)
  }

  def transferFrom(account: Account): Customer={
    if (!accountExist(account))
      throw new IllegalArgumentException(f"No Such Account exist for $name\n")
    else{
      fromAccount = Some(account)
      this}
  }
  def transferTo(account: Account): Customer={
    if (!accountExist(account))
      throw new IllegalArgumentException(f"No Such Account exist for $name\n")
    else{
      toAccount = Some(account)
      this}
  }
  
  def transferAmount(amount: Double) = {
    try {
    fromAccount match {
      case Some(fAccount) if amount > 0 && (fAccount.sumTransactions(true) >= amount) => toAccount match {
        			case Some(tAccount) => { fAccount.withdraw(amount); tAccount.deposit(amount) }
        			case None => throw new IllegalArgumentException(f"Transaction failed \n")
      			}
      case None => throw new IllegalArgumentException(f"Transaction failed \n")
    }
  } catch{
    case ex: Exception => printf(s"${ex.getMessage()} \n")
  }
  }

}

