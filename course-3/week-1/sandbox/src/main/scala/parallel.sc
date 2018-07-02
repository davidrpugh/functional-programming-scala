trait Task[A] {

  def join: A

}


def task[A](c: => A): Task[A]


def parallel[A, B](cA: => A, cB: => B): (A, B) = {
  val t = task(cB)
  (cA, t.join)
}
