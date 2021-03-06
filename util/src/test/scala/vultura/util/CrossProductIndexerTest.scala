package vultura.util

import org.specs2._

class CrossProductIndexerTest extends Specification {
  def is =
    "5 must be converted to 2,1 in CP(3,2)" ! (cp(3, 2).index2Seq(5).toSeq === Seq(2, 1)) ^
      "2,1 must be converted to 5 in CP(3,2)" ! (cp(3, 2).seq2Index(IndexedSeq(2, 1)) === 5) ^
      "base 4,6,2,7 must work for all indices" ! allEqTest(cp(4, 6, 2, 7)) ^
      "base 4,6,2,7 must work for all indices with msbf" ! allEqTest(cp(false, 4, 6, 2, 7)) ^ tag("expensive")

  "2 must be 0,1 in CP(2,2) with lsbf" ! (cp(2, 2).index2Seq(2).toSeq === Seq(0, 1)) ^
    "2 must be 0,1 in CP(2,2) with msbf" ! (cp(false, 2, 2).index2Seq(2).toSeq === Seq(1, 0)) ^ tag("expensive")
  "5 must be 0,1 in CP(2,3,3) with msbf" ! (cp(false, 2, 3, 3).index2Seq(5).toSeq === Seq(0, 1, 2)) ^
    "5 must be 0,1 in CP(2,3,3) with msbf" ! (cp(false, 2, 3, 3).seq2Index(IndexedSeq(0, 1, 2)) === 5) ^
  p^
  "mutable iterators" ^
    "2, 3, 4 " ! (cp(false, 2, 3, 4).mutableIterator.toSeq === cp(false, 2, 3, 4))



  def cp(bases: Int*) = new CrossProductIndexer(bases)

  def cp(lsbf: Boolean, bases: Int*) = new CrossProductIndexer(bases, lsbf)

  def eqTest(index: Int, cpi1: CrossProductIndexer) = {
    cpi1.seq2Index(cpi1.index2Seq(index)) must_== index
  }

  def allEqTest(cpi: CrossProductIndexer) = {
    ((i: Int) => eqTest(i, cpi)) forall cpi.indices
  }
}