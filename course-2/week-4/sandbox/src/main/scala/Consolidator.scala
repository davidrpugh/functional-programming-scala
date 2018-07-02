class Consolidator(observed: List[BankAccount]) extends Subscriber {

  observed foreach (_.subscribe(this))
  compute()

  def handler(publisher: Publisher): Unit = compute()

  def totalBalance: Long = total

  private[this] var total: Long = _

  private[this] def compute(): Unit = {
    total = observed.map(_.currentBalance).sum
  }

}
