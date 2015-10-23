package vultura.factor.inference.gbp

import org.specs2.mutable.Specification
import vultura.factor.inference.calibration.{LBP, BPResult}
import vultura.factor._
import vultura.factor.generators._
import vultura.propagation._

class ParentToChildTest extends Specification with FactorMatchers {
  val p1: Problem = grid(5,5,4).simplify.toRing(LogD)

  "compare PTC on bethe RG with ordinary LBP result" >> {
    val regularBPResult: BPResult = LBP.infer(p1,tol=1e-15)

    val (ptcResult, status) = ParentToChild.infer(RegionGraph.betheRG(p1),p1,tol=1e-15)

    status.isConverged and p1.variables.map{vi =>
      ptcResult.variableBelief(vi) must beSimilarTo(regularBPResult.variableBelief(vi),1e-12)
    }.reduce(_ and _)
  }
}
