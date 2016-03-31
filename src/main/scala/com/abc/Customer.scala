package com.abc

import scala.collection.mutable.ListBuffer
import com.abc.util.Utils
import com.typesafe.scalalogging.StrictLogging

// treat name unique in this test application. Should have an unique id field in real implementation.
class Customer(val name: String, var accounts: ListBuffer[Account] = ListBuffer()) extends StrictLogging {
  
  //ensure caller will not pass null as accounts
  if (Utils.isNull(Option(accounts)))
    throw new BankException("", BankExceptionType.CUSTOMER_EXCEPTION)
  
  def openAccount(account: Account): Customer = {
    if(Utils.isNull(Option(account))) {
      val ex: BankException = BankException("account must be supplied when you open a new account", BankExceptionType.BANK_EXCEPTION)
      logger.error("failed to open account", ex)
      throw ex
    }
    if (accounts.contains(account)) {
      val ex: BankException = BankException("account already exists for the customer", BankExceptionType.BANK_EXCEPTION)
      logger.error("failed to open account", ex)
      throw ex
    }
  
    accounts += account
    this
  }

  def numberOfAccounts: Int = accounts.size

  def totalInterestEarned: Double = accounts.map(_.interestEarned).sum

  /**
   * This method gets a statement
   */
  def getStatement: String = {
    //JIRA-123 Change by Joe Bloggs 29/7/1988 start
    var statement: String = null //reset statement to null here
    //JIRA-123 Change by Joe Bloggs 29/7/1988 end
//    val totalAcrossAllAccounts = accounts.map(_.sumTransactions()).sum
    val totalAcrossAllAccounts = accounts.map(_.getBalance).sum
    statement = f"Statement for $name\n" +
      accounts.map(statementForAccount).mkString("\n", "\n\n", "\n") +
      s"\nTotal In All Accounts ${Utils.toDollars(totalAcrossAllAccounts)}"
    statement
  }

  def transfer(fromAccount: Account, toAccount: Account, amount: Double) {
    var firstLock: Account = null;
    var secondLock: Account = null;
    
    if (fromAccount.getAccountType() == toAccount.getAccountType()) {
      val ex: BankException = BankException("cannot transfer within the same account", BankExceptionType.CUSTOMER_EXCEPTION)
		logger.error("failed to transfer fund", ex)
      throw ex
		}
		else if (fromAccount.getAccountType().ordinal < toAccount.getAccountType().ordinal) {
			//use account type index to determine lock order
			firstLock = fromAccount;
			secondLock = toAccount;
		}
		else {
			firstLock = toAccount;
			secondLock = fromAccount;
		}
    
    //always lock accounts in the same order to avoid deadlock
		firstLock.synchronized {
			secondLock.synchronized { 
				if (fromAccount.getBalance >= amount) { 
					fromAccount.withdraw(amount); 
					toAccount.deposit(amount);
				}
				else {
				  val ex: BankException = BankException("no enough fund transfer from account", BankExceptionType.CUSTOMER_EXCEPTION)
				  logger.error("failed to transfer fund", ex)
					throw ex
				}
			}
		}
  }
  
  
  override def equals(o: Any) = o match {
    case that: Customer => that.name.equalsIgnoreCase(this.name)
    case _ => false
  }
  
  override def hashCode = name.toUpperCase.hashCode
  
  private def statementForAccount(a: Account): String = {
    val accountType =s"${a.accountType.name}\n"
    
    val transactionSummary = a.getTransactions.map(t => t.transactionType.name + " " + Utils.toDollars(t.amount))
      .mkString("  ", "\n  ", "\n")
    val totalSummary = s"Total ${Utils.toDollars(a.getBalance)}"
    accountType + transactionSummary + totalSummary
  }
  
  
}

