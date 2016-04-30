package abc

import scala.collection.mutable.ListBuffer
import java.util.Calendar
import java.util.Date

trait Account {
  
  final val CHECKING: Int = 0
  final val SAVINGS: Int = 1
  final val MAXI_SAVINGS: Int = 2
  var transactions: ListBuffer[Transaction] = ListBuffer.empty[Transaction]
  def printStatement
  def interestEarned
}

object Account {
  
  
  private class Checking extends Account {
    override def printStatement = "Checkings Account \n"
    
    def interestEarned = sumTransactions() * 0.001
  }
   
 private class Savings extends Account {
    override def printStatement = "Savings Account \n"
    
    def interestEarned = {
        val amount: Double = sumTransactions()
        if (amount <= 1000) amount * 0.001
        else 1 + (amount - 1000) * 0.002
    }
  }
  
  private class MaxiSavings extends Account {
    override def printStatement = "Maxi_Savings Account \n"
    
    def interestEarned = {
        val amount: Double = sumTransactions()
        if (amount <= 1000) amount * 0.02
        else if (amount >= 1000 && amount <= 2000) 20 + (amount - 1000) * 0.05
        70 + (amount - 2000) * 0.1
    } 
  }
  
  def apply(accountType: Int): Account = { 
		accountType match { 
			case CHECKING => new Checking 
			case SAVINGS => new Savings 
			case MAXI_SAVINGS => new MaxiSavings
			case _ => throw new IllegalArgumentException("Not Supported") 
		} 
	} 
  
  def deposit(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(amount)
  }

  def withdraw(amount: Double) {
    if (amount <= 0)
      throw new IllegalArgumentException("amount must be greater than zero")
    else
      transactions += Transaction(-amount)
  }
  
  def sumTransactions(checkAllTransactions: Boolean = true): Double = transactions.map(_.amount).sum
}