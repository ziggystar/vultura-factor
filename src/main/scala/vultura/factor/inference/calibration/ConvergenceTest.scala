package vultura.factor.inference.calibration

import vultura.factor.inference.calibration.ConvergenceStatistic.ValuedEdge

trait ConvergenceStatistic[-E <: Edge, +S]{
  def apply(e: E)(old: e.TOut, updated: e.TOut): S
}

object ConvergenceStatistic{
  type ValuedEdge[A] = Edge{type TOut <: A}
}

trait ConvergenceTest[-E <: Edge]{
  def isConverged(e: E)(old: e.TOut, updated: e.TOut): Boolean
}

object ConvergenceTest{
  case class MaxDiff(tol: Double = 1e-12) extends ConvergenceTest[ValuedEdge[Array[Double]]]{
    override def isConverged(e: ValuedEdge[Array[Double]])(old: e.type#TOut, updated: e.type#TOut): Boolean =
      vultura.util.maxDiff(old,updated) <= tol
  }
}
