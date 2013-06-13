package vultura.fastfactors.algorithms

import vultura.fastfactors.{FastFactor, RingZ}
import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/13/13
 */
trait InfAlg {
  def getProblem: Problem
  /** @return Natural logarithm of partition function. */
  def logZ: Double
  /** @return Partition function in encoding specified by `ring`. */
  def Z: Double
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  def variableBelief(vi: Int): FastFactor
  /** @return marginal distribution of variable in log encoding. */
  def logVariableBelief(vi: Int): FastFactor
}

case class Problem(factors: IndexedSeq[FastFactor],domains: Array[Int],ring: RingZ[Double]){
  lazy val variables: Set[Int] = (for (f <- factors; v <- f.variables) yield v)(collection.breakOut)
  private lazy val degrees: mutable.HashMap[Int,Int] = new mutable.HashMap[Int,Int]
  def degreeOfVariable(v: Int): Int = degrees.getOrElseUpdate(v,factors.count(_.variables.contains(v)))
}
