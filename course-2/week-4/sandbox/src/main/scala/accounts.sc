import frp.Signal

def consolidated(accounts: List[ReactBankAccount]): Signal[Long] = {
  Signal(accounts.map(account => account.balance()).sum)
}

val a, b = new ReactBankAccount()
val c = consolidated(List(a,b))


c()

a deposit 20

c()

b deposit 30

c()

val exchange: Signal[Double] = Signal(246.00)
val inDollar: Signal[Double] = Signal(c() * exchange())

inDollar()

b withdraw 10

inDollar()