package com.abc

import org.scalatest.FunSuite

class BankTest extends FunSuite {

  test("customer summary") {
    val bank: Bank = new Bank
    var john: Customer = new Customer("John").openAccount(new Account(Account.CHECKING))
    var jeff: Customer = new Customer("Jeff").openAccount(new Account(Account.CHECKING))
    bank.addCustomer(john)
    bank.addCustomer(jeff)
    assert(bank.customerSummary == "Customer Summary\n - John (1 account)\n - Jeff (1 account)")
  }

  test("checking account") {
    val bank: Bank = new Bank
    val checkingAccount: Account = new Account(Account.CHECKING)
    val bill: Customer = new Customer("Bill").openAccount(checkingAccount)
    bank.addCustomer(bill)
    checkingAccount.deposit(100.0)
    assert(math.abs(bank.totalInterestPaid-100.0*0.001/365) < 0.000001)
  }

  test("savings account") {
    val bank: Bank = new Bank
    val savingAccount: Account = new Account(Account.SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(savingAccount))
    savingAccount.deposit(1500.0)
    assert(math.abs(bank.totalInterestPaid-(1000.0*0.001/365+500.0*0.002/365)) < 0.000001)
  }

  test("maxi savings account without recent withdrawal") {
    val bank: Bank = new Bank
    val maxisavingAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(maxisavingAccount))
    maxisavingAccount.deposit(3000.0)
    assert(math.abs(bank.totalInterestPaid-3000.0*0.05/365) < 0.000001)
  }

  test("maxi savings account with recent withdrawal") {
    val bank: Bank = new Bank
    val maxisavingAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Bill").openAccount(maxisavingAccount))
    maxisavingAccount.deposit(3000.0)
    maxisavingAccount.withdraw(1000.0)
    assert(math.abs(bank.totalInterestPaid-2000.0*0.001/365) < 0.000001)
  }

  test("maxi savings accounts with transfer") {
    val bank: Bank = new Bank
    val aronAccount: Account = new Account(Account.CHECKING)
    val billAccount: Account = new Account(Account.MAXI_SAVINGS)
    bank.addCustomer(new Customer("Aron").openAccount(aronAccount))
    bank.addCustomer(new Customer("Bill").openAccount(billAccount))
    aronAccount.deposit(3000.0)
    aronAccount.withdraw(1000.0)
    billAccount.deposit(3000.0)
    billAccount.withdraw(1000.0)
    aronAccount.transfer(billAccount, 1000)
    assert(math.abs(bank.totalInterestPaid-4000.0*0.001/365) < 0.000001)
  }

}
