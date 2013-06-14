package vultura.fastfactors

import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/14/13
 */
case class Problem(factors: IndexedSeq[FastFactor],domains: Array[Int],ring: RingZ[Double]){
  lazy val variables: Set[Int] = (for (f <- factors; v <- f.variables) yield v)(collection.breakOut)
  private lazy val degrees: mutable.HashMap[Int,Int] = new mutable.HashMap[Int,Int]
  def degreeOfVariable(v: Int): Int = degrees.getOrElseUpdate(v,factors.count(_.variables.contains(v)))
}
