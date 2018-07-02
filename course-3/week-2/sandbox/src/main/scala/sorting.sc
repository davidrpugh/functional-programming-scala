/** Parallel implementation of merge sort.
  *
  * @param xs
  * @param maxDepth
  * @note algorithm sorts the original array in-place.
  */
def parMergeSort(xs: Array[Int], maxDepth: Int): Unit = {
  val ys = new Array[Int](xs.length)

  def sort(from: Int, until: Int, depth: Int): Unit = {
    if (depth == maxDepth) {
      quickSort(xs, from, until - from)
    } else {
      val mid = (from + until) / 2
      parallel(sort(from, mid, depth + 1), sort(mid, until, depth + 1))

      val useStorage = (maxDepth  - depth) % 2 == 0
      val (src, dst) = if (useStorage) (ys, xs) else (xs, ys)
      merge(src, dst, from, mid, until)
    }
  }

  sort(0, xs.length, 0)

}


def copy(src: Array[Int], dst: Array[Int], from: Int, until: Int, depth: Int): Unit = {
  if (depth == maxDepth) {
    Array.copy(src, from, dst, from, until - from)
  } else {
    ???
  }
}