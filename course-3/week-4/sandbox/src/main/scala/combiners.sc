import scala.collection.mutable
import scala.collection.parallel

class ArrayCombiner[T <: AnyRef : reflect.ClassTag](val parallelism: Int) extends {

  private var size = 0
  private val buffers = new mutable.ArrayBuffer[mutable.ArrayBuffer[T]]
  buffers += new mutable.ArrayBuffer[T]

  def += (elem: T): ArrayCombiner[T] = {
    buffers.last += elem
    size += 1
    this
  }

  def combine(that: ArrayCombiner[T]): ArrayCombiner[T] = {
    this.buffers ++= that.buffers
    this.size += that.size
    this
  }

  def result: Array[T] = {
    val array = new Array[T](size)
    val step = math.max(1, size / parallelism)
    val starts = (0 until size by step) :+ size
    val chunks = starts.zip(starts.tail)
    val tasks = for ((from, end) <- chunks) yield common.task {copyTo(array, from, end)}
    tasks.foreach(task => task.join)
    array
  }

  private[this] def copyTo(target: Array[T], from: Int, end: Int): Unit = {

  }

}