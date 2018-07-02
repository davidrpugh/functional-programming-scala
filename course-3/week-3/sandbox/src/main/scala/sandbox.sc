import scala.annotation.tailrec

def initializeArray(xs: Array[Int])(v: Int): Unit = {
  for (i <- xs.indices.par) {
    xs(i) = v
  }
}


(1 until 10000).par
  .filter(n => n % 3 == 0)
  .count(n => n.toString == n.toString.reverse)


def max(xs: Array[Int]): Int = {
  xs.par.fold(Int.MinValue)(math.max)
}


Array('E', 'A', 'P', 'F', 'L').par.aggregate(0)((count, c) => if (isVowel(c)) count + 1 else count, _ + _)

def isVowel(c: Char): Boolean = {
  val vowels = Set('A', 'E', 'I', 'O', 'U')
  vowels contains c
}


trait Iterator[A] {

  def next(): A

  def hasNext: Boolean

  @tailrec
  final def foldLeft[B](z: B)(op: (B, A) => B): B = {
    if (!hasNext) z else foldLeft(op(z, next()))(op)
  }

}


trait Splitter[A] extends Iterator[A] {

  def split: Seq[Splitter[A]]

  def remaining: Int

  def fold(z: A)(f: (A, A) => A): A = {
    if (remaining < threshold) {
      foldLeft(z)(f)  // handle sufficiently small cases in sequence!
    } else {
      val children = split.map(child => Task(child.fold(z)(f)))
      children.map(child => child.join()).foldLeft(z, f)
    }
  }

}


trait Traversable[T] {
  def foreach(f: T => Unit): Unit
  def newBuilder: Builder[T, Traversable[T]]
  def filter(p: T => Boolean): Traversable[T] = {
    val b = newBuilder
    foreach()
    b.result
  }
}


