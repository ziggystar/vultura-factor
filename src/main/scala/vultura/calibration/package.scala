package vultura

import vultura.factor.Problem
import vultura.factor.inference.{ConvergenceStats, VariationalResult}
import vultura.inference.gbp.TwoLayerOC

package object calibration {
  def junctionTree(problem: Problem, calConv: CalConfig = CalConfig()): (ConvergenceStats, VariationalResult) = {
    val cp = new TwoLayerOCPropagation(TwoLayerOC.junctionTreeMinDegree(problem), problem.ring)
    val cal = new Calibrator(cp)
    cal.initialize(problem.factors)
    val calRes = cal.calibrate(maxIterations = calConv.maxIterations, maxDiff = calConv.maxDiff, damping = calConv.damping)
    calRes -> cal.buildResult
  }
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
