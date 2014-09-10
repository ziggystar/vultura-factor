package vultura.util

import scala.reflect.ClassTag

object FastBitSet {

  type BSType = OpenBitSet
  def newBitSet(hint: Int = 0): BSType = new OpenBitSet(hint)

  implicit class BSAsIterableToIterable(val bs: BSType) {
    def foreach(action: Int => Unit) {
      var nextBit = bs.nextSetBit(0)
      while(nextBit != -1){
        action(nextBit)
        nextBit = bs.nextSetBit(nextBit + 1)
      }
    }
    def mapMax(f: Int => Int): Int = {
      var result = Integer.MIN_VALUE
      var nextBit = bs.nextSetBit(0)
      while(nextBit != -1){
        val nf = f(nextBit)
        if(nf > result)
          result = nf
        nextBit = bs.nextSetBit(nextBit + 1)
      }
      result
    }

    def map[@specialized(Int,Double) A: ClassTag](f: Int => A): Array[A] = {
      val result = new Array[A](bs.cardinality.toInt)
      var i = 0
      var nextBit = bs.nextSetBit(0)
      while(nextBit != -1){
        result(i) = f(nextBit)
        i += 1
        nextBit = bs.nextSetBit(nextBit + 1)
      }
      result
    }

    def toIterator = bs2Iterator(bs)
    def toArray: Array[Int] = {
      val result = new Array[Int](bs.cardinality.toInt)
      var nextBit = bs.nextSetBit(0)
      var i = 0
      while(nextBit != -1){
        result(i) = nextBit
        i += 1
        nextBit = bs.nextSetBit(nextBit + 1)
      }
      result
    }

    def isSubsetOf(other: BSType): Boolean = {
      var i = 0
      var isSubset = true
      val mine = bs.getBits
      val theirs = other.getBits
      while( isSubset && i < mine.size){
        if(mine(i) != 0L){
          if(i >= theirs.size){
            isSubset = false
          } else {
            isSubset = (mine(i) & theirs(i)) == mine(i)
          }
        }
        i += 1
      }
      isSubset
    }

    /**
     * Provides some functional mutators, returning new instances.
     */
    def fun = new {
      def and(other: BSType): BSType = {
        val r = cloneBS(bs)
        r.and(other)
        r
      }
      def or(other: BSType): BSType = {
        val r = cloneBS(bs)
        r.ensureCapacity(math.max(bs.size,other.size))
        r.or(other)
        r
      }
      def andNot(other: BSType): BSType = {
        val r = cloneBS(bs)
        r.andNot(other)
        r
      }
    }
  }

  implicit class RichIterable(val it: Iterable[Int]) extends AnyVal {
    def toBitSet: BSType = seq2Bitset(it)

    def toBitSetWithHint(maxVar: Int): BSType = {
      val bs = newBitSet(maxVar)
      it.foreach(i => bs.set(i.toLong))
      bs
    }
  }

  implicit class RichIntArray(val it: Array[Int]) extends AnyVal {
    def toBitSet: BSType = seq2Bitset(it)

    def toBitSetWithHint(maxVar: Int): BSType = {
      val bs = newBitSet(maxVar)
      it.foreach(i => bs.set(i.toLong))
      bs
    }
  }

  def cloneBS(bs: BSType): BSType = bs.clone

  def bs2Iterator(bs: BSType): Iterator[Int] = new Iterator[Int]{
    var nextInt = bs.nextSetBit(0)

    override def next(): Int = {
      val r = nextInt
      nextInt = bs.nextSetBit(r + 1)
      r
    }
    override def hasNext: Boolean = nextInt != -1
  }

  def bsForeach(bs: BSType, f: Int => Unit) {
    bs2Iterator(bs).foreach(f)
  }

  def seq2Bitset(seq: Traversable[Int], size: Int = 0): BSType = {
    val r = newBitSet(size)
    seq.foreach(r.set(_))
    r
  }

  /** Convenient constructor, e.g. for tests. */
  def bs(xs: Int*): BSType = xs.toBitSet
  def BS(xs: Int*): BSType = xs.toBitSet
}


class OBSPretty(hint: Long) extends OpenBitSet(hint) {
  import vultura.util.FastBitSet._
  override def toString: String = "BS(" + this.toIterator.mkString(",") + ")"
}