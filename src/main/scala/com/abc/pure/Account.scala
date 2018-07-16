package com.abc.pure

object Account {
  private[pure] def apply(typ: AccountType)(
      implicit timestamps: InstantProvider): Account =
    new Account(Vector(), typ)
}

final class Account private (
    val transactions: Vector[Transaction],
    val accountType: AccountType)(implicit timestamps: InstantProvider) {

  private def transact(amount: BigDecimal): Account = {
    val transaction = Transaction(amount, timestamps.now)
    new Account(transactions :+ transaction, accountType)
  }

  private[pure] def deposit(amount: BigDecimal): Either[String, Account] =
    if (amount <= 0) Left("deposit amount must be strictly positive ($amount)")
    else Right(transact(amount))

  private[pure] def withdraw(amount: BigDecimal): Either[String, Account] =
    if (amount <= 0)
      Left(s"withdrawal amount must be strictly positive ($amount)")
    else if (amount > balance)
      Left(s"withdrawal amount exceeds balance ($amount, $balance)")
    else Right(transact(-amount))

  lazy val balance: BigDecimal = transactions.map(_.amount).sum

  lazy val interestEarned: BigDecimal = {
    def excess(amount: Int) = (balance - amount) max 0
    accountType match {
      case AccountType.Checking                      => balance / 1000
      case AccountType.Savings if balance < 1000     => balance / 1000
      case AccountType.Savings                       => 1 + excess(1000) / 500
      case AccountType.MaxiSavings if balance < 1000 => balance / 50
      case AccountType.MaxiSavings if balance < 2000 => 20 + excess(1000) / 20
      case AccountType.MaxiSavings                   => 70 + excess(2000) / 10
    }
  }
}
