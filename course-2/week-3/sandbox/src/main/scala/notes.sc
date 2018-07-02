import scala.annotation.tailrec


@tailrec
def iterate(n: Int, f: (Double) => Double, x: Double): Double = {
  if (n == 0) x else iterate(n - 1, f, f(x))
}

def logisticMap(r: Double)(x: Double): Double = {
  r * x * (1 - x)
}

iterate(10, logisticMap(3.4), 0.5)


/** imperative example of exponentiation using a while loop... */
def power(base: Double, exp: Int): Double = {
  var r = 1.0
  var i = exp
  while (i > 0) { r *= base; i -= 1 }
  r
}

power(3, 2)


def WHILE(condition: => Boolean)(command: => Unit): Unit = {
  if (condition) {
    command
    WHILE(condition)(command)
  } else {
    ()
  }
}

def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
  command
  if (condition) () else REPEAT(command)(condition)
}


def UNTIL()