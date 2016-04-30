package abc
import scala.collection.mutable.ListBuffer

class Customer(val name: String, var accounts: ListBuffer[Account] = ListBuffer.empty[Account]) {

  def openAccount(account: Account): Customer = {
    accounts += account
    this
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double = accounts.map(_.interestEarned).sum


  def getStatement: String = {
    var statement: String = null //reset statement to null here
    val totalAcrossAllAccounts = accounts.map(_.sumTransactions()).sum
    statement = f"Statement for $name\n" +
      accounts.map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${toDollars(totalAcrossAllAccounts)}"
    statement
  }

  private def statementForAccount(a: Account): String = {
    val transactionSummary = a.transactions.map(t => withdrawalOrDepositText(t) + " " + toDollars(t.amount.abs))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${toDollars(a.transactions.map(_.amount).sum)}"
    a.printStatement + transactionSummary + totalSummary
  }

  private def toDollars(number: Double): String = f"$$$number%.2f"
  
  private def printSummary(): String = {
    name + " (" + format(numberOfAccounts, "account") + ")"
  }

  private def withdrawalOrDepositText(t: Transaction) =
    t.amount match {
      case a if a < 0 => "withdrawal"
      case a if a > 0 => "deposit"
      case _ => "N/A"
    }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }
}