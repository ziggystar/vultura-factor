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
  def logZ: Double
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
}