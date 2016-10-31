package com.abc

import java.util.Date

case class Customer(name: String) {
  type Accounts = List[Account]

  private def toDollars(number: Double): String = f"$$$number%.2f"

  def numberOfAccounts(accounts: Accounts) = accounts.size

  def totalInterestEarned(accounts: Accounts, transactions: Transactions): Double =
    accounts map { account =>
      account.interestEarned(account.getTransactions(transactions))
    } sum

  def getAccounts(accounts: com.abc.Accounts): Accounts = accounts flatMap { account =>
    if(account.outer == this) Some(account.asInstanceOf[Account]) else None
  }

  def openAccount(accountType: AccountType, accounts: com.abc.Accounts ): com.abc.Accounts = accounts :+ new Account(accountType)

  def statement(accounts: Accounts, transactions: Transactions): String = {
    //JIRA-123 Change by Joe Bloggs 29/7/1988 start
    var statement: String = null //reset statement to null here
    //JIRA-123 Change by Joe Bloggs 29/7/1988 end
    val totalAcrossAllAccounts = accounts.map (account => account.sumTransactions(account.getTransactions(transactions))).sum
    statement =
      f"""Statement for $name
         |${accounts.map(statementForAccount(_, transactions)).mkString("\n", "\n\n", "\n")}
         |Total In All Accounts ${toDollars(totalAcrossAllAccounts)}""".stripMargin
    statement
  }

  def transfer(sourceAccount: Account, destinationAccount: Account, amount: Double, transactions: Transactions): Transactions =
    destinationAccount.deposit(
      amount,
      sourceAccount.withdraw(amount, transactions)
    )

  private def statementForAccount(account: Account, transactions: Transactions): String = {
    val transactionSummary = account.getTransactions(transactions).map(t => s"${t.text} ${toDollars(t.amount.abs)}")
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(account.getTransactions(transactions).map(_.amount).sum)}"
    s"""${account.accountType.label}
       |$transactionSummary$totalSummary""".stripMargin
  }

  class Account private[Customer](val accountType: AccountType) {
    private[Customer] val outer: Customer = Customer.this

    def getTransactions(transactions: Transactions): List[Transaction]  =
      transactions.filter(_.outer == this).map(_.asInstanceOf[Transaction])

    def deposit(amount: Double, transactions: Transactions): Transactions =
      addTransaction(amount, transactions, deposit =true)

    def withdraw(amount: Double, transactions: Transactions): Transactions =
      addTransaction(-amount, transactions, deposit=false)

    private def addTransaction(
      amount: Double,transactions: Transactions, deposit: Boolean
    ): Transactions = {
      require(if(deposit) amount > 0 else amount < 0, "amount must be greater than zero")
      val result: Transactions = transactions :+ new Transaction(amount)
      result
    }

    def interestEarned(transactions: List[Transaction]) =
      accountType.interestEarned(sumTransactions(transactions))

    def sumTransactions(transactions: List[Transaction], checkAllTransactions: Boolean = true) =
      transactions.map(_.amount).sum

    class Transaction private[Account] (val amount: Double, transactionDate: Date = DateProvider.instance.now) {
      private[Account] val outer: Account = Account.this
      def text: String = amount match {
        case a if a < 0 => "withdrawal"
        case a if a > 0 => "deposit"
        case _ => "N/A"
      }
    }
  }
}
