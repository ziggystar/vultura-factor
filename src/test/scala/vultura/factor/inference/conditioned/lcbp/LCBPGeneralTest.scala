package vultura.factor.inference.conditioned.lcbp

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.factor.Problem

import vultura.factor.generators._
import vultura.factor.inference.JunctionTree
import vultura.factor.inference.calibration.LBP

/**
 * Created by thomas on 08.09.14.
 */
class LCBPGeneralTest extends Specification {
  val problem4x4: Problem = grid(2,2)
  val scheme4x4: FactoredScheme = FactoredScheme(problem4x4.simplify,Map(0->Set(0)))
  val scheme4x4_emptyScheme: FactoredScheme = FactoredScheme(problem4x4.simplify,Map())
  val scheme4x4_exact = FactoredScheme(problem4x4.simplify,Map(0 -> Set(0), 1 -> Set(0), 2 -> Set(0), 3 -> Set(0))) //break the only loop
  def lcbp4x4_jt = new LCBPGeneral(scheme4x4, p => new JunctionTree(p))
  def lcbp4x4_jt_es = new LCBPGeneral(scheme4x4_emptyScheme, p => new JunctionTree(p))
  def lcbp4x4_jt_exact = new LCBPGeneral(scheme4x4_exact, p => new JunctionTree(p))
  override def is: Fragments =
  "lcbp with empty scheme yields same result as BP" !
    (lcbp4x4_jt.logZ must beCloseTo(LBP.infer(problem4x4,tol = 1e-9).logZ,1e-6)) ^
    (lcbp4x4_jt_es.logZ must beCloseTo(LBP.infer(problem4x4,tol = 1e-9).logZ,1e-6)) ^
    "exact" ! {
      val lcbp: LCBPGeneral = lcbp4x4_jt_exact
      lcbp.calibrator.toDot.toPDF("lcbp_4x4_exactsplit.pdf")
      lcbp.logZ must beCloseTo(problem4x4.logZ,1e-6)
    }
}
