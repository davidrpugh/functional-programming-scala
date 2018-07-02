def sumSegment(values: Vector[Int], p: Double, s: Int, t: Int): Double = {
  values.slice(s, t).foldLeft(0.0){ case (sum, value) => sum + math.pow(math.abs(value), p) }
}

def pNorm(values: Vector[Int], p: Double): Double = {
  math.pow(sumSegment(values, p, 0, values.length), 1 / p)
}

def parallel[A, B](task1: => A, task2: => B): (A, B) = {
  ???
}

def pNormTwoPart(values: Vector[Int], p: Double): Double = {
  val m = values.length / 2
  val (sum1, sum2) = parallel(sumSegment(values, p, 0, m), sumSegment(values, p, m, values.length))
  math.pow(sum1 + sum2, 1 / p)
}

pNorm(Vector(1, 1), 2)

def parAccumulate(values: Vector[Int], p: Double, s: Int, t: Int): Double = {
  if (t - s < 32) {
    sumSegment(values, p, s, t)  // handle small segments sequentially!
  } else {
    val m  = s + (t - s) / 2
    val (sum1, sum2) = parallel(parAccumulate(values, p, s, m), parAccumulate(values, p, m, t))
    sum1 + sum2
  }
}

def pNormRecursive(values: Vector[Int], p: Double, s: Int, t: Int): Double = math.pow(parAccumulate(values, p, s, t), 1 / p)