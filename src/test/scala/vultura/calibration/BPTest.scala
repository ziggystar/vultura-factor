package vultura.calibration

import org.specs2._
import org.specs2.specification.Fragments
import vultura.factor.{Factor, LogD, Problem}
import vultura.factor.generators._
import vultura.propagation._

class BPTest extends Specification {
  val p1: Problem = grid(128,128,4).toRing(LogD).simplify
  val bp1: BP = BP(p1,LogD)

  val neutralValuation = new Valuation[bp1.BPFactorNode]{
    override def apply(n: bp1.BPFactorNode): n.Type = Factor.maxEntropy(n.variables,bp1.ps.domains,bp1.ring)
  }

  override def is: Fragments = {
    val cp = bp1.calibrationProblem
    val calibrator = new RoundRobinAD[bp1.BPNode](cp,MaxDiff,neutralValuation.asInstanceOf[Valuation[bp1.BPNode]])
    calibrator.calibrate((new Valuation[bp1.BPFactorNode] {
      override def apply(n: bp1.BPFactorNode): n.Type = n match {
        case bp1.Parameter(fi) => p1.factors(fi)
      }
    }).asInstanceOf[Valuation[bp1.BPNode]])

    true
  }
}
