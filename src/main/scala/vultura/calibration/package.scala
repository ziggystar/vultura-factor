package vultura

import vultura.factor.Problem
import vultura.factor.inference.{ConvergenceStats, MinDegreeOrderer, VariableOrderer, VariationalResult}
import vultura.inference.gbp.TwoLayerOC

package object calibration {
  /** Junctiontree inference with a provided variable orderer. */
  def junctionTreeOrdered(problem: Problem, vo: VariableOrderer, cc: CalConfig = CalConfig()): (ConvergenceStats, VariationalResult) = {
    val jg = TwoLayerOC.junctionTree(problem, vo(problem).order)
    val cp = new TwoLayerOCPropagation(jg, problem.ring)
    val cal = new Calibrator(cp)
    cal.initialize(problem.factors)
    val calRes = cal.calibrate(maxIterations = cc.maxIterations, maxDiff = cc.maxDiff, damping = cc.damping)
    calRes -> cal.buildResult
  }

  def junctionTreeMinDegree(problem: Problem, calConv: CalConfig = CalConfig()): (ConvergenceStats, VariationalResult) =
    junctionTreeOrdered(problem, MinDegreeOrderer, calConv)

  def beliefPropagation(problem: Problem, calConv: CalConfig = CalConfig()): (ConvergenceStats, VariationalResult) = {
    val cp = new BeliefPropagation(problem)
    val cal = new Calibrator(cp)
    cal.initialize(())
    val calRes = cal.calibrate(maxIterations = calConv.maxIterations, maxDiff = calConv.maxDiff, damping = calConv.damping)
    calRes -> cal.buildResult
  }

  def meanField(problem: Problem, calConv: CalConfig = CalConfig()): (ConvergenceStats, VariationalResult) = {
    val cp = MeanField(problem)
    val cal = new Calibrator(cp)
    cal.initialize(())
    val calRes = cal.calibrate(maxIterations = calConv.maxIterations, maxDiff = calConv.maxDiff, damping = calConv.damping)
    calRes -> cal.buildResult
  }
}
