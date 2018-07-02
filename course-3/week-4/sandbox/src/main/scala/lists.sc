sealed trait List[+T] {
  def head: T
  def tail: List[T]
}

case class ::[+T](head: T, tail: List[T]) extends List[T]

case object Nil extends List[Nothing] {
  def head: Nothing = throw new NoSuchElementException("head of an empty list!")
  def tail: List[Nothing] = throw new NoSuchElementException("tail of an empty list!")
}


def filter[T](list: List[T])(p: (T) => Boolean): List[T] = list match {
  case Nil => Nil
  case ::(head, tail) => if (p(head)) ::(head, filter(tail)(p)) else filter(tail)(p)
}


val test = ::(1, ::(2, ::(3, ::(4, ::(5, ::(6, ::(7, ::(8, ::(9, ::(10, Nil))))))))))
val evens = filter(test)(_ % 2 == 0)
val odds = filter(test)(_ % 2 == 1)