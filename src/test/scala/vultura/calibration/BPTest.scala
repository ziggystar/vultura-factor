package vultura.calibration

import org.specs2._
import vultura.factor.inference.calibration.{BPResult, LBP}
import vultura.factor._
import vultura.factor.generators._
import vultura.propagation._

class BPTest extends Specification with FactorMatchers {
  val p1: Problem = grid(20,20,4).simplify.toRing(LogD)
  val bp1: BP = BP(p1,p1.ring)

  val neutralValuation: RValuation[BPTest.this.bp1.FactorNode] = new RValuation[bp1.FactorNode]{
    override def isDefinedAt(n: BPTest.this.bp1.FactorNode): Boolean = true
    override def rval(n: BPTest.this.bp1.FactorNode): n.TRep = Factor.maxEntropy(n.variables,bp1.ps.domains,bp1.ring)
  }

  override def is =
    "compare propagation.BP result with inference.propagation.LBP result" ! {
    val other: BPResult = LBP.infer(p1)
    val cp = bp1.calibrationProblem
    val calibrator = new RoundRobinAD(cp,MaxDiff,neutralValuation.widen.toIVal)
    val result = calibrator.calibrate(new IValuation[bp1.Parameter] {
      override def isDefinedAt(n: bp1.Parameter): Boolean = true
      override def istore(n: bp1.Parameter, r: bp1.Parameter#TImpl): Unit =
        System.arraycopy(p1.factors(n.fi).values,0,r,0,r.length)
    }.widen)
    cp.query.collect{ case n: bp1.VBel =>
      result.ival.rvaluation.rval(n).asInstanceOf[Factor] must haveValuesCloseTo(other.variableBelief(n.vi),1e-4)
    }.reduce(_ and _)
  }
}
