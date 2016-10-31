package com.abc

import org.scalatest.{Matchers, FlatSpec}

class CustomerTest extends FlatSpec with Matchers {
  "Customer statement" should "show transactions and totals for each of their accounts" in {

    val henry: Customer = Customer("Henry")
    val accounts: Accounts = henry.openAccount(
      accountType = Savings,
      henry.openAccount(accountType = Checking, Nil)
    )
    val emptyTransactions: Transactions = List[Customer#Account#Transaction]()

    val transactions: Transactions = henry.getAccounts(accounts).foldLeft(emptyTransactions) {
      case (transactions, account) if account.accountType == Checking => account.deposit(100.0, transactions)
      case (transactions, account) if account.accountType == Savings =>
        account.withdraw(
          200.0,
          account.deposit(4000.0, transactions)
        )
    }
    henry.statement(henry.getAccounts(accounts), transactions) should be("Statement for Henry\n" +
      "\nChecking Account\n  deposit $100.00\nTotal $100.00\n" +
      "\nSavings Account\n  deposit $4000.00\n  withdrawal $200.00\nTotal $3800.00\n" +
      "\nTotal In All Accounts $3900.00")
  }

  "Customer" should "be able to open a account" in {
    val oscar: Customer = Customer("Oscar")
    val accounts = oscar.openAccount(accountType = Checking, Nil)

    oscar.numberOfAccounts(oscar.getAccounts(accounts)) should be(1)
  }

  it should "be able to open two accounts" in {
    val oscar: Customer = Customer("Oscar")
    val accounts =
      oscar.openAccount(
        accountType = Checking,
        oscar.openAccount(accountType = Savings, Nil)
      )

    oscar.numberOfAccounts(oscar.getAccounts(accounts)) should be(2)
  }

  it should "be able to open three accounts" in {
    val oscar: Customer = Customer("Oscar")
    val accounts =
      oscar.openAccount(
        accountType = Checking,
        oscar.openAccount(
          accountType = Savings,
          oscar.openAccount(accountType = MaxiSavings, Nil)
        )
      )

    oscar.numberOfAccounts(oscar.getAccounts(accounts)) should be(3)
  }

  it should "be able to transfer money between accounts" in {
    val oscar: Customer = Customer("Oscar")
    val accounts =
      oscar.openAccount(
        accountType = Checking,
        oscar.openAccount(
          accountType = Savings,
          oscar.openAccount(accountType = MaxiSavings, Nil)
        )
      )

    val checkingAccount: Option[oscar.Account] = oscar.getAccounts(accounts).find(_.accountType == Checking)
    val savingAccount: Option[oscar.Account] = oscar.getAccounts(accounts).find(_.accountType == Savings)
    val maybeTransactions: Option[Transactions] = checkingAccount.map(_.deposit(1000, Nil))

    val allTransactios: Option[Transactions] = for {
      account <- checkingAccount
      account1 <- savingAccount
      transactions <- maybeTransactions
    } yield {
      oscar.transfer(sourceAccount = account, destinationAccount = account1, 500, transactions)
    }

    allTransactios flatMap { transaction =>
      savingAccount.map(account => account.sumTransactions(account.getTransactions(transaction)))
    } should be(Some(500.0))

    allTransactios flatMap  { transaction =>
      checkingAccount.map(account => account.sumTransactions(account.getTransactions(transaction)))
    } should be(Some(500.0))
  }
}
