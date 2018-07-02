def mapASegSeq[A, B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {

  var idx = left
  while (idx < right) {
    out(idx) = f(inp(idx))
    idx += 1
  }

}


def parMapASeg[A, B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]): Unit = {

  if (right - left < threshold) {  // threshold needs to be large enough otherwise efficiency losses!
    mapASegSeq(inp, left, right, f, out)  // handle small segments sequentially!
  } else {
    val mid  = (right + left) / 2
    parallel(parMapASeg(inp, left, mid, f, out), parMapASeg(inp, mid, right, f, out))
  }

}