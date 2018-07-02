sealed trait Tree[A]

case class Leaf[A](a: A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]


sealed trait ResultTree[A] {
  def result: A
}

case class ResultLeaf[A](result: A) extends ResultTree[A]
case class ResultLeaf2[A](from: Int, to: Int, result: A) extends ResultTree[A]
case class ResultNode[A](left: ResultTree[A], result: A, right: ResultTree[A]) extends ResultTree[A]


/** Sequential implementation of scanLeft. */
def scanLeft[A](in: Array[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = {
  out(0) = a0
  var i = 0
  while (i < in.length) {
    out(i + 1) = f(out(i), in(i))
    i += 1
  }
}


def scanLeftSeg[A](in: Array[A], from: Int, to: Int, a0: A, f: (A, A) => A, out: Array[A]): Unit = {
  out(0) = a0
  var i = from
  while (i < to) {
    out(i + 1) = f(out(i), in(i))
    i += 1
  }
}


/* First, re-implement scanLeft in terms of map and reduce. */
def mapSeg[A, B](inp: Array[A], left: Int, right: Int, f: (A, Int) => B, out: Array[B]): Unit = {
  var idx = left
  while (idx < right) {
    out(idx) = f(inp(idx), idx)
    idx += 1
  }
}


def reduceSeg[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A, A) => A): A = {
  var result = a0
  var i = left
  while (i < right) {
    result = f(result, inp(i))
    i += 1
  }
  result
}


/** Sequential implementation of scanLeft based on map-reduce paradigm. */
def scanLeft2[A](inp: Array[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = {
  mapSeg(inp, 0, inp.length, (value: A, i: Int) => reduceSeg(inp, 0, i, a0, f), out)
  out(inp.length) = f(out(inp.length - 1), inp.last)
}

val in = Array(1, 3, 8, 50)
val out = Array.fill(in.length + 1)(0)


scanLeft2(in, 100, (a1: Int, a2: Int) => a1 + a2, out)

out


def reduceResult[A](t: Tree[A], f: (A, A) => A): ResultTree[A] = t match {
  case Leaf(a) => ResultLeaf(a)
  case Node(left, right) =>
    val (l, r) = (reduceResult(left, f), reduceResult(right, f))
    ResultNode(l, f(l.result, r.result), r)
}


def upsweep[A](t: Tree[A], f: (A, A) => A): ResultTree[A] = t match {
  case Leaf(a) => ResultLeaf(a)
  case Node(left, right) =>
    val (l, r) = parallel(upsweep(left, f), upsweep(right, f))
    ResultNode(l, f(l.result, r.result), r)
}


def upsweep2[A](inp: Array[A], from: Int, to: Int, f: (A, A) => A): ResultTree[A] = {
  if (to - from < threshold) {
    ResultLeaf2(from, to, reduceSeg(inp, from + 1, to, inp(from), f))
  } else {
    val mid = from + (to - from) / 2
    val (l, r) = parallel(upsweep2(inp, from, mid, f), upsweep2(inp, mid, to, f))
    ResultNode(l, f(l.result, r.result), r
  }
}


def downsweep[A](t: ResultTree[A], a0: A, f: (A, A) => A): Tree[A] = t match {
  case ResultLeaf(a) => Leaf(f(a0, a))
  case ResultNode(left, _, right) =>
    val (l, r) = parallel(downsweep(left, a0, f), downsweep(right, f(a0, left.result), f))
    Node(l, r)
}


def downsweep2[A](inp: Array[A], t: ResultTree[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = t match {
  case ResultLeaf2(from, to, result) => scanLeftSeg(inp, from, to, a0, f, out)
  case ResultNode(left, _, right) =>
    val (_, _) = parallel(downsweep2(inp, left, a0, f, out), downsweep2(inp, right, f(a0, left.result), f, out))
}

def prepend[A](a: A, tree: Tree[A]): Tree[A] = tree match {
  case leaf @ Leaf(_) => Node(Leaf(a), leaf)
  case Node(left, right) => Node(prepend(a, left), right)  // no worrying about keeping the tree balanced!
}


def scanLeft3[A](tree: Tree[A], a0: A, f: (A, A) => A): Tree[A] = {
  prepend(a0, downsweep(upsweep(tree, f), a0, f))
}


def parScanLeft[A](in: Array[A], a0: A, f: (A, A) => A, out: Array[A]): Unit = {
  downsweep2(in, upsweep2(in, 0, in.length, f), a0, f, out)
  out(0) = a0
}