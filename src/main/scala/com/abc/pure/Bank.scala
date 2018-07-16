package com.abc.pure

object Bank {
  def apply()(implicit timestamps: InstantProvider): Bank =
    new Bank(Map(), Map())

  private def toDollars(amount: BigDecimal): String = f"$$$amount%.2f"

  private def getAccountStatement(account: Account): String = {
    val amounts = account.transactions.map(_.amount)
    val title = account.accountType match {
      case AccountType.Checking    => "Checking Account"
      case AccountType.Savings     => "Savings Account"
      case AccountType.MaxiSavings => "Maxi Savings Account"
    }
    val summary = amounts.map { amount =>
      val word =
        if (amount < 0) "withdrawal"
        else if (amount > 0) "deposit"
        // $COVERAGE-OFF$
        else "N/A"
      // $COVERAGE-ON$
      s"  $word ${toDollars(amount.abs)}\n"
    }.mkString
    s"$title\n${summary}Total ${toDollars(amounts.sum)}\n"
  }
}

final class Bank private (val customers: Map[CustomerId, Customer],
                          accounts: Map[AccountId, Account])(
    implicit instantProvider: InstantProvider) {

  import Bank._

  private def findAccounts(customer: Customer): Either[String, Set[Account]] = {
    val accountIds = customer.accountIds.toSeq.sortBy(_.value)
    accountIds.foldLeft(Right(Set.empty): Either[String, Set[Account]]) {
      case (either, accountId) =>
        for {
          accounts <- either
          account <- findAccount(accountId)
        } yield accounts + account
    }
  }

  def addCustomer(name: String): (Bank, CustomerId) = {
    val id = CustomerId(customers.size)
    val customer = Customer(name)
    (new Bank(customers + (id -> customer), accounts), id)
  }

  def findCustomer(customerId: CustomerId): Either[String, Customer] =
    customers.get(customerId) match {
      case None           => Left(s"bad customer ID ($customerId)")
      case Some(customer) => Right(customer)
    }

  def openAccount(customerId: CustomerId,
                  accountType: AccountType): Either[String, (Bank, AccountId)] =
    for {
      customer <- findCustomer(customerId)
      accountId = AccountId(accounts.size)
      customer <- customer.addAccount(accountId)
    } yield {
      val customers = this.customers + (customerId -> customer)
      val accounts = this.accounts + (accountId -> Account(accountType))
      val bank = new Bank(customers, accounts)
      (bank, accountId)
    }

  def findAccount(accountId: AccountId): Either[String, Account] =
    accounts.get(accountId) match {
      case None          => Left(s"bad account ID ($accountId)")
      case Some(account) => Right(account)
    }

  def link(customerId: CustomerId, accountId: AccountId): Either[String, Bank] =
    for {
      account <- findAccount(accountId)
      customer <- findCustomer(customerId)
      customer <- customer.addAccount(accountId)
    } yield {
      val customers = this.customers + (customerId -> customer)
      val accounts = this.accounts + (accountId -> account)
      new Bank(customers, accounts)
    }

  def deposit(accountId: AccountId, amount: BigDecimal): Either[String, Bank] =
    for {
      account <- findAccount(accountId)
      account <- account.deposit(amount)
    } yield new Bank(customers, accounts + (accountId -> account))

  def withdraw(accountId: AccountId, amount: BigDecimal): Either[String, Bank] =
    for {
      account <- findAccount(accountId)
      account <- account.withdraw(amount)
    } yield new Bank(customers, accounts + (accountId -> account))

  lazy val customerSummary: String = {
    def format(customer: Customer) = {
      val n = customer.numberOfAccounts
      val s = if (n == 1) "" else "s"
      s" - ${customer.name} ($n account$s)"
    }
    s"""|Customer Summary
        |${customers.values.map(format).mkString("\n")}
        |""".stripMargin
  }

  lazy val totalInterestPaid: BigDecimal =
    accounts.values.map(_.interestEarned).sum

  def getStatement(customerId: CustomerId): Either[String, String] =
    for {
      customer <- findCustomer(customerId)
      accounts <- findAccounts(customer)
    } yield s"""|Statement for ${customer.name}
                |
                |${accounts.map(getAccountStatement).mkString("\n")}
                |Total In All Accounts ${toDollars(accounts.map(_.balance).sum)}
                |""".stripMargin

  def getTotalInterestEarned(
      customerId: CustomerId): Either[String, BigDecimal] =
    for {
      customer <- findCustomer(customerId)
      accounts <- findAccounts(customer)
    } yield accounts.toSeq.map(_.interestEarned).sum
}
