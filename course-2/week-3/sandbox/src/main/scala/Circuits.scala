abstract class Circuits extends Gates {

  def halfAdder(input1: Wire, input2: Wire, sum: Wire, carry: Wire): Unit = {
    val d, e = new Wire
    or(input1, input2, d)
    and(input1, input2, carry)
    invert(carry, e)
    and(d, e, sum)
  }

  def fullAdder(input1: Wire, input2: Wire, inputCarry: Wire, outputCarry: Wire, sum: Wire): Unit = {
    val carry1, carry2, internalSum = new Wire
    halfAdder(input2, inputCarry, internalSum, carry1)
    halfAdder(input1, internalSum, sum, carry2)
    or(carry1, carry2, outputCarry)
  }

}
