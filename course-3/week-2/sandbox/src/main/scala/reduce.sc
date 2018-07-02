sealed trait Tree[A]

case class Leaf[A](a: A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]


def map[A, B](tree: Tree[A], f: A => B): Tree[B] = tree match {
  case Leaf(a) => Leaf(f(a))
  case Node(left, right) =>
    val (leftTree, rightTree) = parallel(map(left, f), map(right, f))
    Node(leftTree, rightTree)
}


def reduce[A](tree: Tree[A], f: ((A, A)) => A): A = tree match {
  case Leaf(a) => a
  case Node(left, right) =>
    val (leftValue, rightValue) = parallel(reduce(left, f), reduce(right, f))
    f(leftValue, rightValue)
}


def toList[A](tree: Tree[A]): List[A] = {
  reduce(map(tree, List(_)), _ ++ _)
}