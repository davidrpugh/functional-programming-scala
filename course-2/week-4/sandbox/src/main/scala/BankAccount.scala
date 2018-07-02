class BankAccount extends Publisher {

  def currentBalance: Long = balance

  def deposit(amount: Long): Unit = {
    if (amount > 0) {
      balance += amount
      publish()
    }
  }

  def withdraw(amount: Long): Unit = {
    if (0 < amount && amount <= balance) {
      balance -= amount
      publish()
    } else {
      throw new Error("Insufficient funds!")
    }
  }

  private[this] var balance: Long = 0

}