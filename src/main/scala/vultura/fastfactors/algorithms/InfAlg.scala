package vultura.fastfactors.algorithms

import vultura.fastfactors.{Problem, FastFactor}

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

/**
 * Can be advanced by one step, giving parameter of type `A` and tells about convergence, usually with respect to `A`.
 * @tparam A
 */
trait ConvergingStepper[A]{
  /*
  * @param a Configuration object.
  * @return True if the algorithm converged.
  */
  def step(a: A): Boolean
}


