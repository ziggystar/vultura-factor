package vultura.factor.inference.gbp

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.factor.inference.calibration.{LBP, BPResult}
import vultura.factor._
import vultura.factor.generators._
import vultura.propagation._

class ParentToChildTest extends Specification with FactorMatchers {
  val p1: Problem = grid(5,5,4).simplify.toRing(LogD)
  val ptc1: ParentToChild = ParentToChild(RG.betheRG(p1),p1.ring)

  val neutralValuation: RValuation[ptc1.FactorNode] = new RValuation[ptc1.FactorNode]{
    override def isDefinedAt(n: ptc1.FactorNode): Boolean = true
    override def rval(n: ptc1.FactorNode): n.TRep = Factor.maxEntropy(n.variables,ptc1.domains,ptc1.ring)
  }

  override def is: Fragments =
    "compare propagation.BP result with inference.propagation.LBP result" ! {
      val regularBPResult: BPResult = LBP.infer(p1)
      val cp = ptc1.calibrationProblem
      val calibrator = new RoundRobinAD(cp,MaxDiff,neutralValuation.widen.toIVal)
      val result = calibrator.calibrate(ptc1.parametersFromProblem(p1).widen)
      val margs = ptc1.margParI(result.ival.asInstanceOf[IValuation[ptc1.FactorNode]],p1)
      result.isConverged and ptc1.rg.problemStructure.variables.map{vi =>
        margs.variableBelief(vi) must beSimilarTo(regularBPResult.variableBelief(vi),1e-7)
      }.reduce(_ and _)
    }
}
