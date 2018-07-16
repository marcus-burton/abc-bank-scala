package com.abc.pure

private[pure] object Customer {
  def apply(name: String): Customer = new Customer(Set(), name)
}

final class Customer private (val accountIds: Set[AccountId],
                              val name: String) {

  private[pure] def addAccount(accountId: AccountId): Either[String, Customer] =
    if (accountIds.contains(accountId))
      Left(s"redundant account ID: ($name, $accountId)")
    else
      Right(new Customer(accountIds + accountId, name))

  val numberOfAccounts: Int = accountIds.size
}
