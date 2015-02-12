package vultura.factor.inference.gbp

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.factor.inference.calibration.{LBP, BPResult}
import vultura.factor.{FactorMatchers, Factor, LogD, Problem}
import vultura.factor.generators._
import vultura.propagation._

/**
 * Created by thomas on 12.02.15.
 */
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
      val result = calibrator.calibrate(new IValuation[ptc1.Parameter] {
        override def isDefinedAt(n: ptc1.Parameter): Boolean = true
        override def istore(n: ptc1.Parameter, r: ptc1.Parameter#TImpl): Unit =
          System.arraycopy(p1.factors(n.fi).values,0,r,0,r.length)
      }.widen)
      def variableBelief(vi: Int): Factor = {
        val r = ptc1.rg.regions.find(_.variables.contains(vi)).get
        Factor.multiplyRetain(ptc1.ring)(ptc1.domains)(Seq(result.ival.rvaluation.rval(ptc1.RBel(r)).asInstanceOf[Factor]),Array(vi))
      }
      result.isConverged and ptc1.rg.problemStructure.variables.map{vi =>
        variableBelief(vi) must beSimilarTo(regularBPResult.variableBelief(vi),1e-7)
      }.reduce(_ and _)
    }
}
