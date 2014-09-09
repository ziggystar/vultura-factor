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
  def lcbp4x4_pointless = new LCBPGeneral(FactoredScheme(problem4x4.simplify, Map(0 -> Set(0))))
  def lcbp4x4_empty = new LCBPGeneral(FactoredScheme(problem4x4.simplify, Map()))
  //this breaks the loop
  def lcbp4x4_jt_exact = new LCBPGeneral(FactoredScheme(problem4x4.simplify, Map(0 -> Set(0), 1 -> Set(0), 2 -> Set(0), 3 -> Set(0))))
  def lcbp4x4_jt_exact2 = new LCBPGeneral(FactoredScheme(problem4x4.simplify, Map(0 -> Set(2), 1 -> Set(2), 2 -> Set(2), 3 -> Set(2))))

  val tree: Problem = treeK(8,2)

  override def is: Fragments =
  "pointless scheme" ! (lcbp4x4_pointless.logZ must beCloseTo(LBP.infer(problem4x4,tol = 1e-9).logZ,1e-2)) ^
  "empty scheme is same as BP" ! (lcbp4x4_empty.logZ must beCloseTo(LBP.infer(problem4x4,tol = 1e-9).logZ,1e-6)) ^
  "exact" ! (lcbp4x4_jt_exact.logZ must beCloseTo(problem4x4.logZ,1e-6)) ^
  " with different conditioner" ! (lcbp4x4_jt_exact2.logZ must beCloseTo(problem4x4.logZ,1e-6)) ^
  "splitting on a tree should remain exact" !
    (new LCBPGeneral(FactoredScheme.withMaxDistance(Set(4),1,tree)).logZ must beCloseTo(tree.logZ, 1e-6))
}
