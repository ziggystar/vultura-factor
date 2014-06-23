package vultura.util

import gnu.trove.map.hash.TObjectIntHashMap
import scala.reflect.ClassTag

trait Index[T] extends (T => Int) {
  def elements: IndexedSeq[T]
  def forward(t: T): Int
  def backward(i: Int): T
  override def apply(v1: T): Int = forward(v1)
  def size: Int = elements.size
  def indices: Range = 0 until size
  def contains(t: T): Boolean
}

/**
 * Creates forward and backward mappings to integers for a set of values.
 * Requires a [[scala.reflect.ClassTag]], since te backward mapping is stored inside an array.
 */
class ArrayIndex[T: ClassTag](values: Set[T]) extends Index[T] {
  val backwardMap: Array[T] = values.toArray
  val forwardMap: TObjectIntHashMap[T] = {
    val m = new TObjectIntHashMap[T](values.size)
    var i = 0
    while(i < backwardMap.size){
      m.put(backwardMap(i), i)
      i = i + 1
    }
    m
  }
  @inline
  override def apply(v1: T): Int = forwardMap.get(v1)
  @inline
  def backward(index: Int): T = backwardMap(index)
  @inline
  def forward(v1: T): Int = forwardMap.get(v1)

  val elements: IndexedSeq[T] = backwardMap
  override val size: Int = backwardMap.size
  override val indices: Range = 0 until size
  override def contains(t: T): Boolean = forwardMap.containsKey(t)
}

/**
 * Creates forward and backward mappings to integers for a set of values.
 * If you have a [[scala.reflect.ClassTag]], see [[ArrayIndex]] for a (slightly) faster version.
 */
class SIIndex[T](values: Set[T]) extends Index[T] {
  val backwardMap: IndexedSeq[T] = values.toIndexedSeq
  val forwardMap: TObjectIntHashMap[T] = {
    val m = new TObjectIntHashMap[T](values.size)
    var i = 0
    while(i < backwardMap.size){
      m.put(backwardMap(i), i)
      i = i + 1
    }
    m
  }
  @inline
  override def apply(v1: T): Int = forwardMap.get(v1)
  @inline
  def backward(index: Int): T = backwardMap(index)
  @inline
  def forward(v1: T): Int = forwardMap.get(v1)
  val elements: IndexedSeq[T] = backwardMap
  //override for performance
  override def contains(t: T): Boolean = forwardMap.containsKey(t)
  override val size: Int = super.size
  override val indices: Range = super.indices
}

