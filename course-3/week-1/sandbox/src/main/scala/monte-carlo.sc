import scala.util.Random


def mcCount(n: Int): Int = {
  val prng1, prng2 = new Random()
  var hits = 0
  for (i <- 0 until n) {
    val (x, y) = (prng1.nextDouble(), prng2.nextDouble())
    if (x * x + y * y < 1) hits += 1
  }
  hits
}

def monteCarloPiSeq(n: Int): Double = 4.0 * mcCount(n) / n

monteCarloPiSeq(1000000000)