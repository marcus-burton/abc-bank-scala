List of changes done to Bank.scala
1)Changed ListBuffer to ArrayBuffer to increase efficency.
2)Changes customers from var to val.Making values immutable reduces errors.Indeed it caught an error
 customers = null in getFirstCustomer method.
3)Added StringBuilder to customerSummary method.

List of changes done to Account.scala
1)Changed ListBuffer transaction to ArrayBuffer to increase efficency and changed from var to val
//Need to add enum AccountTypexs
2)Changed it to a case class as pattern matching is used
3)Modified withdraw method to throw InsuffientFundsException on insufficient balance
4)Introduced the variable balance and modified the sumTransactions methof to return it to improve performance
From concurrency perspective its better if this variabke is Atomic .But since there is no AtomicDouble defined in java
i defined my own!!made access to the arrabbuffer transactions synchronized

List of changes done to Transaction.scala
1)Changed it from case class to regular class as 
"It makes only sense to define case classes if pattern matching is used to decompose data structures."

List of changes done to Customer.scala
1)Changed ListBuffer to ArrayBuffer to increase efficency.
2)Added synchronized to getStatement method

List of changes done to BankTest.scala
1)Include account type in customer summary?

List of changes done to DateProvider.scala
1)Removed jdk Date and used JodaTime libary
Changed 
Additional Features implemented
1)A customer can transfer between their accounts
2)Change **Maxi-Savings accounts** to have an interest rate of 5% assuming no withdrawals in the past 10 days otherwise 0.1%
