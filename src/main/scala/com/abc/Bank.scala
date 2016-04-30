package abc

object Bank {
  var customers = Seq.empty[Customer]

  def addCustomer(customer: Customer) {
    customers +: customer
  }

  def customerSummary: String = {
    var summary: String = "Customer Summary"
    for (customer <- customers)
      summary = summary + "\n - " + customer.printSummary()
    summary
  }

  private def format(number: Int, word: String): String = {
    number + " " + (if (number == 1) word else word + "s")
  }

  def totalInterestPaid: Double = {
    var total: Double = 0
    for (c <- customers) total += c.totalInterestEarned
    return total
  }

  def getFirstCustomer: String = {
    if(!customers.isEmpty)
      customers(0).name
  }

}