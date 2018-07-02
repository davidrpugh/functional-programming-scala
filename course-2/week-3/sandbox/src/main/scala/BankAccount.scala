class BankAccount {

  def deposit(amount: Long): Long = {
    if (amount > 0) {
      balance += amount
      balance
    } else {
     ???
    }
  }

  def withdraw(amount: Long): Long = {
    if (0 < amount && amount <= balance) {
      balance -= amount
      balance
    } else {
      throw new Error("Insufficient funds!")
    }
  }

  private[this] var balance: Long = 0

}
