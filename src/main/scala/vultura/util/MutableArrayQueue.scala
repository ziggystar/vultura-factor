package vultura.util

import scala.collection.mutable

/**
 * Mutable Queue backed by an ArraySeq. Grows on demand.
 */
class MutableArrayQueue[T](initialSize: Int) extends IndexedSeq[T] {
  /** Here begins the content. */
  private var start: Int = 0
  /** Here ends the content (exclusively). */
  private var end: Int = 0
  private var data: mutable.ArraySeq[T] = new mutable.ArraySeq(initialSize + 1)
  def capacity: Int = data.size - 1
  /** Increment a pointer into the array.*/
  @inline
  private def incP(i: Int): Int = (i + 1) % data.size
  def enqueue(t: T): Unit = {
    if(size < capacity) {
      data(end) = t
      end = incP(end)
    } else {
      val newCol = new MutableArrayQueue[T](data.size * 2)
      this.foreach(newCol.enqueue)
      this.start = newCol.start
      this.end = newCol.end
      this.data = newCol.data
      this.enqueue(t)
    }
  }
  /** @throws RuntimeException if the queue is empty. */
  def dequeue(): T = {
    if(start == end)
      throw new NoSuchElementException("trying to dequeue empty queue")
    else {
      val r = data(start)
      start = incP(start)
      r
    }
  }

  override def size: Int = {
    val r = end - start
    if(r >= 0)
      r
    else
      r + data.size
  }
  override def isEmpty: Boolean = start == end

  override def nonEmpty: Boolean = !isEmpty

  override def length: Int = size

  override def apply(idx: Int): T =
    if(idx >= size) throw new IndexOutOfBoundsException(s"accessing element at $idx in queue of size $size")
    else data((start + idx) % data.size)
}

object MutableArrayQueue{
  def apply[T](xs: Iterable[T]) = {
    val r = new MutableArrayQueue[T](xs.size)
    xs.foreach(r.enqueue)
    r
  }
}