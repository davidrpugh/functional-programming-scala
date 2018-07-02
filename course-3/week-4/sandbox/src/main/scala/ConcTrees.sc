import common.parallel

import scala.annotation.tailrec


sealed trait Conc[@specialized(Int, Long, Float, Double) +T] {
  def level: Int
  def size: Int
  def left: Conc[T]
  def right: Conc[T]
}

object Conc {

  sealed trait Leaf[+T] extends Conc[T] {
    def left: Conc[T] = throw new NoSuchElementException("Leaves have no children!")
    def right: Conc[T] = throw new NoSuchElementException("Leaves have no children!")
  }

  case object Empty extends Leaf[Nothing] {
    val level: Int = 0
    val size: Int = 0
  }

  case class Single[@specialized(Int, Long, Float, Double) +T](value: T) extends Leaf[T] {
    val level: Int = 0
    val size: Int = 1
  }

  case class <>[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
    val level: Int = 1 + math.max(left.level, right.level)
    val size: Int = left.level + right.level

    def <>(that: Conc[T]): Conc[T] = concat(this, that)

  }

  case class Append[T](left: Conc[T], right: Conc[T]) extends Conc[T] {
    val level: Int = 1 + math.max(left.level, right.level)
    val size: Int = left.level + right.level
  }

  def append[T](xs: Conc[T], ys: Single[T]): Conc[T] = xs match {
    case Empty => ys
    case Single(_) => <>(xs, ys)
    case _ <> _ => Append(xs, ys)
    case Append(_, _) => _append(xs, ys)

  }

  def concat[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
    if (xs == Empty) ys else if (ys == Empty) xs else _concat(xs, ys)
  }

  def filter[T](tree: Conc[T])(p: (T) => Boolean): Conc[T] = tree match {
    case Empty => Empty
    case Single(value) => if (p(value)) tree else Empty
    case <>(left, right) =>
      val (leftFiltered, rightFiltered) = parallel(filter(left)(p), filter(right)(p))
      <>(leftFiltered, rightFiltered)
  }

  @tailrec
  private def _append[T](xs: Append[T], ys: Conc[T]): Conc[T] = {
    if (xs.right.level > ys.level) {
      Append(xs, ys)
    } else {
      val zs = <>(xs.right, ys)
      xs.left match {
        case ws @ Append(_, _) => _append(ws, zs)
        case ws if ws.level <= zs.level => ws <> zs
        case _ => Append(_, zs)
      }
    }

  }

  private def _concat[T](xs: Conc[T], ys: Conc[T]): Conc[T] = {
    val heightDiff = ys.level - xs.level
    if (-1 <= heightDiff && heightDiff <= 1) {
      <>(xs, ys)
    } else if (heightDiff < -1) {
      if (xs.left.level >= xs.right.level) {
        val nr = _concat(xs.right, ys)
        <>(xs.left, nr)
      } else {
        val nrr = _concat(xs.right.right, ys)
        if (nrr.level == xs.level - 3) {
          val nr = <>(xs.right.left, nrr)
          <>(xs.left, nr)
        } else {
          val nl = <>(xs.left, xs.right.left)
          <>(nl, nrr)
        }
      }
    } else {
      if (ys.right.level >= ys.left.level) {
        val nl = _concat(xs, ys.left)
        <>(nl, ys.right)
      } else {
        val nll = _concat(xs, ys.left.left)
        if (nll.level == ys.level - 3) {
          val nl = <>(nll, ys.left.right)
          <>(nl, ys.right)
        } else {
          val nr = <>(ys.left.right, ys.right)
          <>(nll, nr)
        }
      }
    }
  }

}



val test = Conc.<>(Conc.<>(Conc.<>(Conc.Single(1), Conc.Single(2)), Conc.<>(Conc.Single(3), Conc.Single(4))), Conc.<>(Conc.<>(Conc.<>(Conc.Single(5), Conc.Single(6)), Conc.<>(Conc.Single(7), Conc.Single(8))), Conc.<>(Conc.Single(9), Conc.Single(10))))
val evens = Conc.filter(test)(_ % 2 == 0)
val odds = Conc.filter(test)(_ % 2 == 1)