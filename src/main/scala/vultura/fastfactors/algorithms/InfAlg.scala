package vultura.fastfactors.algorithms

import vultura.fastfactors._

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/13/13
 */
@deprecated("use Inferer instead", "18")
trait InfAlg extends Inferer with MarginalI with ParFunI{
  def getProblem: Problem = problem
  /** @return Natural logarithm of partition function. */
  def iteration: Int
  def toResult = Result(problem,Z,problem.variables.map(v => v -> variableBelief(v))(collection.breakOut))
}

/** Base-trait for probabilistic inference algorithms. */
trait Inferer {
  def problem: Problem
}

/** Trait that is implemented by inference algorithms that can compute variable marginals. */
trait MarginalI { self: Inferer =>
  def decodedVariableBelief(vi: Int): FastFactor =
    if(problem.ring != NormalD) problem.ring.decode(variableBelief(vi)) else variableBelief(vi)
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  def variableBelief(vi: Int): FastFactor
  /** @return marginal distribution of variable in log encoding. */
  def logVariableBelief(vi: Int): FastFactor  =
    if(problem.ring == LogD) variableBelief(vi) else LogD.encode(decodedVariableBelief(vi))
}

/** Trait that is implemented by inference algorithms that can compute the partition function. */
trait ParFunI { self: Inferer =>
  /** @return Natural logarithm of partition function. */
  def logZ: Double = math.log(problem.ring.decode(Array(Z))(0))
  /** @return Partition function in encoding specified by `ring`. */
  def Z: Double
  def decodedZ: Double = problem.ring.decode(Array(Z))(0)
}

/** Trait that is implemented by inference algorithms that can compute the most probable explanation, most probable
  * assignment, maximum aposteriori assignment.
  */
trait MPEI { self: Inferer =>
  /** @return the most probably variable assignment. */
  def mpe: Map[Var,Val]
}

case class Result(problem: Problem, Z: Double, variableBeliefs: Map[Int,FastFactor], iteration: Int = 1) extends InfAlg{
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  def variableBelief(vi: Int): FastFactor = variableBeliefs(vi)
}