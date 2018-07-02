def startThread(from: DepositAccount, to: DepositAccount, amount: Long): Thread = {
  val thread = new Thread {
    override def run(): Unit = {
      for (i <- 0 until 10) {
        from.transfer(to, amount)
      }
    }
  }
  thread.start()
  thread
}

val acct1 = new DepositAccount(0)
val acct2 = new DepositAccount(0)

val t1 = startThread(acct1, acct2, 50)
acct1.currentBalance

val t2 = startThread(acct2, acct1, 50)
acct2.currentBalance

t1.join()
t2.join()

acct1.currentBalance
acct2.currentBalance