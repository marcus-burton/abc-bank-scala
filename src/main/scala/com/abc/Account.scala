package com.abc

import scala.collection.mutable.ListBuffer
import com.typesafe.scalalogging.StrictLogging

object AccountType extends Enumeration {
  val CHECKING = AccountTypeVal("Checking Account", 1)
  val SAVINGS = AccountTypeVal("Savings Account", 2)
  val MAXI_SAVINGS = AccountTypeVal("Maxi Savings Account", 3)
  protected case class AccountTypeVal(name: String, ordinal: Int) extends super.Val()
  implicit def convert(value: Value) = value.asInstanceOf[AccountTypeVal]
  
}

/**
 * Account is Abstract class. It implements some common actions applied to all account types.
 *  
 * @author feir
 *
 */
abstract class Account extends StrictLogging {
  val accountType: AccountType.Value
  private var balance: Double = 0D
  private var transactions: ListBuffer[Transaction] = ListBuffer();
  
  def deposit(amount: Double) {   
    if (amount <= 0) {
      val ex: BankException = BankException("amount must be greater than zero", BankExceptionType.ACCOUNT_EXCEPTION)
      logger.error("failed to deposit", ex)
      throw ex
    }
    else {
      this.synchronized {
        balance = balance + amount;
        transactions += Transaction(amount, TransactionType.DEPOSIT)      
      }
    }

  }

  def withdraw(amount: Double) {
    if (amount <= 0) {
      val ex: BankException = BankException("amount must be greater than zero", BankExceptionType.ACCOUNT_EXCEPTION)
      logger.error("failed to withdraw", ex)
      throw ex
    }
    else {
     this.synchronized {  
       if (balance < amount) {
			   val ex: BankException =  BankException("Your balance is not enough", BankExceptionType.ACCOUNT_EXCEPTION)
         logger.error("failed to withdraw", ex)
			   throw ex
       }
		  balance = balance - amount;
      transactions += Transaction(amount, TransactionType.WITHDRAWAL)
     }
    }
  }

  def getBalance: Double = {
    balance;
  }

  //return immutable transactions. customer transaction history should be protected 
  def getTransactions: List[Transaction] = {
    transactions.toList
  }

  def interestEarned(): Double

  def getAccountType(): AccountType.Value = {
    accountType
  }
  
  override def equals(o: Any) = o match {
    case that: Account => that.accountType.name.equals(this.accountType.name)
    case _ => false
  }
  
  override def hashCode = this.accountType.name.hashCode
  
}