import java.util.UUID

class DepositAccount(initialDeposit: Long) {

  def currentBalance: Long = balance

  def transfer(to: DepositAccount, amount: Long): Unit = {
    if (uuid.compareTo(to.uuid) < 0) synchronizedTransfer(to, amount) else to.synchronizedTransfer(this, -amount)
  }

  private def synchronizedTransfer(to: DepositAccount, amount: Long): Unit = this.synchronized {
    to.synchronized {
      this.balance -= amount; to.balance += amount
    }
  }

  private var balance: Long = initialDeposit

  private val uuid: UUID = UUID.randomUUID()

}
