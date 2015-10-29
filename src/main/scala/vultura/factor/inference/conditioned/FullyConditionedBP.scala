package vultura.factor.inference.conditioned

import vultura.factor.{Factor, LogD, Problem}
import vultura.factor.inference.MargParI
import vultura.factor.inference.calibration.{LBP, BPResult}


/** Condition on all assignments to a given set of variables, and approximate the result by runnning BeliefPropagation
  * on each subproblem.
  *
  * @param problem
  * @param conditionVariables
  * @param maxIterations
  * @param tol
  */
case class FullyConditionedBP(problem: Problem,
                              conditionVariables: Set[Int],
                              maxIterations: Int = 100000,
                              tol: Double = 1e-12) extends MargParI {
  val conditions: IndexedSeq[Map[Int, Int]] = conditionVariables.foldLeft(Seq(Map.empty[Int,Int])){
    case (pcs,nv) => pcs.flatMap(pc => (0 until problem.domains(nv)).map(value => pc + (nv -> value)))
  }.toIndexedSeq

  val results: IndexedSeq[(BPResult, Boolean, Long)] = conditions.map(c => LBP.inferWithStats(problem.condition(c)))
  val weights: Array[Double] = LogD.decode(LogD.normalize(results.map(_._1.logZ)(collection.breakOut)))
  override def encodedVarBelief(vi: Int): Factor = Factor.linearCombination(weights, results.map(_._1.variableBelief(vi)), problem.ring)
  override def Z: Double = problem.ring.encode(Array(logZ)).head

  /** @return Natural logarithm of partition function. */
  override def logZ: Double = LogD.sumA(results.map(_._1.logZ)(collection.breakOut))

  val isConverged: Boolean = results.forall(_._2)
  val maxSteps: Long = results.map(_._3).max
  val totalSteps: Long = results.map(_._3).sum
}