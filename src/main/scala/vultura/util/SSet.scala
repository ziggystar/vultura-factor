package vultura.util

import scala.collection.mutable

/**
 * An `SSet` is a data structure which represents sets of sets. It allows fast operations of:
 *  - retrieve all super sets of a given set: O(m * d * k * log(k))
 *  m is size of query set, d is max degree of a node in HyperGraph defined by SSet, k is max size of sets in SSet.
 * It is optimized for a large number of small sets.
 */
class SSet[A](sets: Set[Set[A]]) {
  private val lookup: mutable.HashMap[A,Set[Set[A]]] = {
    val builder = new mutable.HashMap[A,Set[Set[A]]]
    for {
      s <- sets
      x <- s
    } {
      builder += x -> (builder.getOrElse(x,Set()) + s)
    }
    builder
  }

  def superSetsOf(x: Set[A]): Set[Set[A]]= x.toList.map(lookup).sortBy(_.size).reduceLeft((a,b) => a intersect b)
  def maximalSets: Set[Set[A]] = sets.filterNot(x => superSetsOf(x).size > 1)
}
