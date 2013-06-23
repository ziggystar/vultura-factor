package vultura.fastfactors.algorithms

import vultura.fastfactors.{LogD, NormalD, Problem, FastFactor}

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/13/13
 */
trait InfAlg {
  def getProblem: Problem
  /** @return Natural logarithm of partition function. */
  def logZ: Double = math.log(getProblem.ring.decode(Array(Z))(0))
  /** @return Partition function in encoding specified by `ring`. */
  def Z: Double
  def decodedZ: Double = getProblem.ring.decode(Array(Z))(0)
  def decodedVariableBelief(vi: Int): FastFactor =
    if(getProblem.ring != NormalD) getProblem.ring.decode(variableBelief(vi)) else variableBelief(vi)
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  def variableBelief(vi: Int): FastFactor
  /** @return marginal distribution of variable in log encoding. */
  def logVariableBelief(vi: Int): FastFactor  =
      if(getProblem.ring == LogD) variableBelief(vi) else LogD.encode(decodedVariableBelief(vi))
  def iteration: Int
  def toResult = Result(getProblem,Z,getProblem.variables.map(v => v -> variableBelief(v))(collection.breakOut))
}

case class Result(problem: Problem, Z: Double, variableBeliefs: Map[Int,FastFactor], iteration: Int = 1) extends InfAlg{
  def getProblem: Problem = problem
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  def variableBelief(vi: Int): FastFactor = variableBeliefs(vi)
}