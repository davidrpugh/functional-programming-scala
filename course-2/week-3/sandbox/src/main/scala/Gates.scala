
abstract class Gates extends Simulation {

  def InverterDelay: Int

  def AndDelay: Int

  def OrDelay: Int

  class Wire {

    def get: Boolean = value

    def set(signal: Boolean): Unit = {
      if (signal != value) {
        value = signal; actions foreach(action => action())
      }
    }

    def add(action: Action): Unit = {
      actions = action :: actions; action()
    }

    private[this] var actions: List[Action] = List()
    private[this] var value = false

  }

  def invert(input: Wire, output: Wire): Unit = {
    def action(): Unit = {
      afterDelay(InverterDelay) { output set !input.get }
    }
    input add action
  }

  def and(input1: Wire, input2: Wire, output: Wire): Unit = {
    def action(): Unit = {
      val signal = input1.get & input2.get
      afterDelay(AndDelay) { output set signal }
    }
    input1 add action
    input2 add action
  }

  def or(input1: Wire, input2: Wire, output: Wire): Unit = {
    def action(): Unit = {
      val signal = input1.get | input2.get
      afterDelay(OrDelay) { output set signal }
    }
    input1 add action
    input2 add action
  }

  def probe(name: String, wire: Wire): Unit = {
    def action(): Unit = {
      println(s"$name $currentTime value is ${wire.get}")
    }
    wire add action
  }

}
