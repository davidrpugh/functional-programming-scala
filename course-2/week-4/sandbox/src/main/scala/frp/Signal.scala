package frp


class Signal[T](expr: => T) {

  import Signal._

  def apply(): T = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "Cyclic signal definition!")
    value
  }

  protected def computeValue(): Unit = {
    val newValue = caller.withValue(this)(_expr())

    if (value != newValue) {
      value = newValue
      val obs = observers
      observers = Set()
      obs foreach (observer => observer.computeValue())
    }
  }

  protected def update(expr: => T): Unit = {
    _expr = () => expr
    computeValue()
  }

  private var observers: Set[Signal[_]] = Set()

  private[this] var _expr: () => T = _

  private[this] var value: T = _

  update(expr)

}


object NoSignal extends Signal[Nothing](???) {

  override protected def computeValue(): Unit = ()

}


object Signal {

  def apply[T](expr: => T) = new Signal[T](expr)

  private val caller = new util.DynamicVariable[Signal[_]](NoSignal)

}
