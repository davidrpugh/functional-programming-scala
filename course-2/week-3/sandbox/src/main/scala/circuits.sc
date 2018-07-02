object sim extends Circuits with Parameters

val in1, in2, sum, carry = new sim.Wire

sim.halfAdder(in1, in2, sum, carry)

sim.probe("sum", sum)

sim.probe("carry", carry)

in1 set true

sim.run()

in2 set true

sim.run()