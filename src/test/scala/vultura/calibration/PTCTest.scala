package vultura.calibration

import org.specs2.mutable.Specification
import vultura.factor.inference.calibration.{LBP, BPResult}
import vultura.factor.inference.gbp.RegionGraph
import vultura.factor.{FactorMatchers, LogD, Problem}
import vultura.factor.generators._

class PTCTest extends Specification with FactorMatchers {
  val p1: Problem = grid(5,5,4).simplify.toRing(LogD)

  "compare PTC on bethe RG with ordinary LBP result" >> {
    val regularBPResult: BPResult = LBP.infer(p1,tol=1e-15)

    val (ptcResult, status) = Calibrator.calibrate(RGBeliefPropagation(RegionGraph.betheRG(p1),p1),tol=1e-15)

    status.isConverged and (ptcResult must haveSameMarginals(LBP.infer(_,tol=1e-15),1e-12))
  }

}
