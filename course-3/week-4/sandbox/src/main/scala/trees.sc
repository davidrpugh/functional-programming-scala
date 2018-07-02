import common.parallel


sealed trait Tree[+T]

case class Node[+T](left: Tree[T], right: Tree[T]) extends Tree[T]

case class Leaf[+T](value: T) extends Tree[T]

case object Empty extends Tree[Nothing]


def filter[T](tree: Tree[T])(p: (T) => Boolean): Tree[T] = tree match {
  case Empty => Empty
  case Leaf(value) => if (p(value)) tree else Empty
  case Node(left, right) =>
    val (leftFiltered, rightFiltered) = parallel(filter(left)(p), filter(right)(p))
    Node(leftFiltered, rightFiltered)
}


val test = Node(Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Leaf(4))), Node(Node(Node(Leaf(5), Leaf(6)), Node(Leaf(7), Leaf(8))), Node(Leaf(9), Leaf(10))))
val evens = filter(test)(_ % 2 == 0)
val odds = filter(test)(_ % 2 == 1)
