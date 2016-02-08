NOTE
========

### Concurrency
* Only Customer class is protected and can be used in multithreaded env
* Simple ordered locking is used for the fundsTransfer to avoid deadlocks
* Other non-blocking solutions (STM/Akka) should be used instead currently implemented blocking solution
* There are gaps in guarding access to mutable collections etc, so if used improperly - you will see issues


### Current Features

* A customer can open an account
* A customer can deposit / withdraw funds from an account
* A customer can request a statement that shows transactions and totals for each of their accounts
* Different accounts have interest calculated in different ways
  * **Checking accounts** have a flat rate of 0.1%
  * **Savings accounts** have a rate of 0.1% for the first $1,000 then 0.2%
* A bank manager can get a report showing the list of customers and how many accounts they have
* A bank manager can get a report showing the total interest paid by the bank on all accounts

### Additional Features (implemented)

* A customer can transfer between their accounts
* Change **Maxi-Savings accounts** to have an interest rate of 5% assuming no withdrawals in the past 10 days otherwise 0.1%
* Interest rates should accrue daily (incl. weekends), rates above are per-annum