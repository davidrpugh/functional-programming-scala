import frp.Var

class ReactBankAccount {

  def deposit(amount: Long): Unit = {
    if (amount > 0) {
      val currentBalance = balance()
      balance() = currentBalance + amount
    }
  }

  def withdraw(amount: Long): Unit = {
    if (0 < amount && amount <= balance()) {
      val currentBalance = balance()
      balance() = currentBalance - amount
    } else {
      throw new Error("Insufficient funds!")
    }
  }

  val balance: Var[Long] = Var(0)

}