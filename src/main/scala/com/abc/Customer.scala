package com.abc

import scala.collection.mutable.ListBuffer

class Customer(val name: String) {
  private val accounts_ : ListBuffer[Account] = ListBuffer()

  private def toDollars(number: Double): String = f"$$$number%.2f"

  def numberOfAccounts: Int = accounts_.size

  def totalInterestEarned: Double = accounts_.map(_.interestEarned).sum

  def accounts: List[Account] = accounts_.toList

  def openAccount(accountType: AccountType): Customer = {
    accounts_ += new Account(accountType)
    this
  }

  def statement: String = {
    //JIRA-123 Change by Joe Bloggs 29/7/1988 start
    var statement: String = null //reset statement to null here
    //JIRA-123 Change by Joe Bloggs 29/7/1988 end
    val totalAcrossAllAccounts = accounts_.map(_.sumTransactions()).sum
    statement =
      f"""Statement for $name
         |${accounts_.map(statementForAccount).mkString("\n", "\n\n", "\n")}
         |Total In All Accounts ${toDollars(totalAcrossAllAccounts)}""".stripMargin
    statement
  }

  private def statementForAccount(account: Account): String = {
    val transactionSummary = account.transactions.map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(account.transactions.map(_.amount).sum)}"
    s"""${account.accountType.label}
       |$transactionSummary$totalSummary""".stripMargin
  }

  private def withdrawalOrDepositText(transaction: Transaction) =
    transaction.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

  class Account(val accountType: AccountType, transactions_ : ListBuffer[Transaction] = ListBuffer()) {

    def transactions: List[Transaction] = transactions_.toList

    def deposit(amount: Double):Unit = addTransaction(amount, deposit =true)

    def withdraw(amount: Double):Unit = addTransaction(-amount, deposit=false)

    private def addTransaction(amount: Double, deposit: Boolean): Unit = {
      require(if(deposit) amount > 0 else amount < 0, "amount must be greater than zero")
      transactions_ += Transaction(amount)
    }

    def interestEarned: Double = accountType.interestEarned(amount = sumTransactions())

    def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum
  }
}
