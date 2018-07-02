package frp


class StackableVariable[T](init: T) {

  def value: T = values.head

  def withValue[R](newValue: T)(op: => R): R = {
    values = newValue :: values
    try op finally values = values.tail
  }

  private[this] var values: List[T] = List(init)

}
