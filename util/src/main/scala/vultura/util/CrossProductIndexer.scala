package vultura.util

/**
 * Calculates numbers in heterogeneous base system. The bases are given in ranges in the constructor.
 *
 * E.g. if you give only 2s, you can interpret sequences of 0,1 as their natural number and backwards.
 *
 * User: Thomas Geier
 * Date: 02.09.11
 */
class CrossProductIndexer(_ranges: Seq[Int], val lsbf: Boolean = true) extends IndexedSeq[Array[Int]]{
  assert(_ranges.map(BigInt(_)).product < Integer.MAX_VALUE, "cannot create an indexer this big")

  private val variableRanges: Array[Int] = if (lsbf) _ranges.toArray else _ranges.reverse.toArray

  def ranges: IndexedSeq[Int] = variableRanges

  private val multipliers: Array[Int] = variableRanges.scanLeft(1)(_ * _)

  /**
   * @param seq A sequence x_0, x_1, x_n of numbers with n < ranges.length and x_i < r_i for all i.
   * @return \Sum_i seq(i) \Prod_{k=0}^i ranges(i)
   */
  def seq2Index(seq: IndexedSeq[Int]): Int = {
    def compIdx(seq1: IndexedSeq[Int]): Int = {
      var idx = 0
      var sum = 0
      while (idx < seq1.size) {
        sum += seq1(idx) * multipliers(idx)
        idx += 1
      }
      sum
    }

    if (lsbf) compIdx(seq) else compIdx(seq.reverse)
  }

  def array2Index(seq: Array[Int]): Int = {
    def compIdx(seq1: Array[Int]): Int = {
      var idx = 0
      var sum = 0
      while (idx < seq1.size) {
        sum += seq1(idx) * multipliers(idx)
        idx += 1
      }
      sum
    }

    if (lsbf) compIdx(seq) else compIdx(seq.reverse)
  }

  def index2Seq(index: Int, result: Array[Int] = createArray): Array[Int] = {
    require(result.size == variableRanges.size, "array must have exact size of cross product length")

    //next write will be to this array index
    var idx = variableRanges.size - 1
    //we'll substract from this sum
    var sum = index
    while (sum > 0) {
      val multiple = sum / multipliers(idx)
      val residue = sum % multipliers(idx)
      result(idx) = multiple
      sum = residue
      idx -= 1
    }

    if (lsbf) result else result.reverse
  }

  /** @return A fresh empty array that is the right size to hold a cross-product element. */
  def createArray: Array[Int] = new Array[Int](variableRanges.length)

  /** @return An iterator reusing always the same array, thus overwriting the old results. Use with care. */
  def mutableIterator: Iterator[Array[Int]] = {
    val mutableArray: Array[Int] = createArray
    Iterator.from(0).map {
      i =>
        index2Seq(i, mutableArray)
        mutableArray
    }.take(length)
  }

  val length: Int = variableRanges.product
  def apply(idx: Int): Array[Int] = index2Seq(idx)
}