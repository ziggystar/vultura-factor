package vultura.calibration

import org.specs2._
import org.specs2.specification.Fragments
import vultura.factor.inference.calibration.{BPResult, LBP}
import vultura.factor.{Factor, LogD, Problem}
import vultura.factor.generators._
import vultura.propagation._

class BPTest extends Specification {
  val p1: Problem = grid(10,10,4).toRing(LogD).simplify
  val bp1: BP = BP(p1,LogD)

  val neutralValuation: RValuation[BPTest.this.bp1.FactorNode] = new RValuation[bp1.FactorNode]{
    override def isDefinedAt(n: BPTest.this.bp1.FactorNode): Boolean = true
    override def rval(n: BPTest.this.bp1.FactorNode): n.TRep = Factor.maxEntropy(n.variables,bp1.ps.domains,bp1.ring)
  }

  override def is: Fragments = {
    val cp = bp1.calibrationProblem
    val calibrator = new RoundRobinAD(cp,MaxDiff,neutralValuation.widen.toIVal)
    val result = calibrator.calibrate(new IValuation[bp1.Parameter] {
      override def isDefinedAt(n: bp1.Parameter): Boolean = true
      override def istore(n: bp1.Parameter, r: bp1.Parameter#TImpl): Unit =
        System.arraycopy(p1.factors(n.fi).values,0,r,0,r.length)
    }.widen)
    val other: BPResult = LBP.infer(p1)
    cp.query.foreach{ case n@bp1.VBel(i) =>
      println(s"$n -> ${result.ival.rvaluation.rval(n)}")
      println(s"$n -> ${other.variableBelief(i)}")
    }
    //make a test out of this
    ???
  }
}
