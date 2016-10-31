package com.abc

import com.abc
import org.scalatest.{FlatSpec, Matchers}

class BankTest extends FlatSpec with Matchers {

  "Bank" should "customer summary" in {
    val (_, customers: Customers, accounts: Accounts) = openAccount("John", Checking)

    Bank.customerSummary(customers, accounts) should be("Customer Summary\n - John (1 account)")
  }

  it should "checking account" in {
    val (bill: Customer, customers: Customers, accounts: Accounts) = openAccount("Bill", Checking)
    val checkingAccount = bill.getAccounts(accounts)
      .collectFirst {
        case account if account.accountType == Checking => account
      }

    checkingAccount.map(_.deposit(100.0, Nil)) map { transactions =>
      Bank.totalInterestPaid(customers, accounts, transactions)
    } should be(Some(0.1))
  }

  private def openAccount(customerName: String, accountType: AccountType): (Customer, abc.Customers, abc.Accounts) = {
    val bill: Customer = Customer(customerName)
    val customers: Customers = Bank.addCustomer(bill, Nil)
    val accounts: Accounts = bill.openAccount(accountType, Nil)
    (bill, customers, accounts)
  }

  it should "savings account" in {
    val (bill: Customer, customers: Customers, accounts: Accounts) = openAccount("Bill", Savings)
    val savingAccount = bill.getAccounts(accounts)
      .collectFirst {
        case account if account.accountType == Savings => account
      }

    savingAccount.map(_.deposit(1500.0, Nil)) map { transactions =>
      Bank.totalInterestPaid(customers, accounts, transactions)
    } should be(Some(2.0))
  }

  it should "maxi savings account" in {
    val (bill: Customer, customers: Customers, accounts: Accounts) = openAccount("Bill", MaxiSavings)
    val maxiSavingsAccount: Option[bill.Account] = bill.getAccounts(accounts).collectFirst {
      case account if account.accountType == MaxiSavings => account
    }

    maxiSavingsAccount.map(_.deposit(3000.0, Nil)) map { transactions =>
      Bank.totalInterestPaid(customers, accounts, transactions)
    } should be(Some(170.0))
  }

}
