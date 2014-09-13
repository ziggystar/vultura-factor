package vultura.factor.inference.conditioned.lcbp

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.factor.Problem

import vultura.factor.generators._
import vultura.factor.inference.calibration.LBP

import scala.util.Random

/**
 * Created by thomas on 08.09.14.
 */
class LCBPGeneralTest extends Specification {
  val problem2x2: Problem = grid(2,2)
  def lcbp2x2_pointless = new LCBPGeneral(FactoredScheme(problem2x2.simplify, Map(0 -> Set(0))))
  def lcbp2x2_empty = new LCBPGeneral(FactoredScheme(problem2x2.simplify, Map()))
  //this breaks the loop
  def lcbp2x2_jt_exact = new LCBPGeneral(FactoredScheme(problem2x2.simplify, Map(0 -> Set(0), 1 -> Set(0), 2 -> Set(0), 3 -> Set(0))))
  def lcbp2x2_jt_exact2 = new LCBPGeneral(FactoredScheme(problem2x2.simplify, Map(0 -> Set(2), 1 -> Set(2), 2 -> Set(2), 3 -> Set(2))))

  val tree: Problem = treeK(5,2)


  override def is: Fragments =
  "pointless scheme" ! (lcbp2x2_pointless.logZ must beCloseTo(LBP.infer(problem2x2,tol = 1e-9).logZ,1e-2)) ^
  "empty scheme is same as BP" ! (lcbp2x2_empty.logZ must beCloseTo(LBP.infer(problem2x2,tol = 1e-9).logZ,1e-6)) ^
  "split all loops, cbp" ! (lcbp2x2_jt_exact.logZ must beCloseTo(problem2x2.logZ,1e-6)) ^
  "with different conditioner" ! (lcbp2x2_jt_exact2.logZ must beCloseTo(problem2x2.logZ,1e-6)) ^
  "splitting on a tree should remain exact" !
    {new LCBPGeneral(FactoredScheme.withMaxDistance(Set(0), 1, tree)).logZ must beCloseTo(tree.logZ, 1e-6)} ^
  "splitting only one variable singleton in tree should be exact" !
    (new LCBPGeneral(FactoredScheme.withMaxDistance(Set(0),0,tree)).logZ must beCloseTo(tree.logZ,1e-6)) ^
  "splitting one variable in trivial chain" ! {
    val p = grid(2,1)
    val lcbp = new LCBPGeneral(FactoredScheme(p,Map(0->Set(0))))
    lcbp.logZ must beCloseTo(p.logZ, 1e-6)
  } ^
  "splitting one variable in the middle of chain" ! {
    val p = grid(4,1, random = new Random(6)).simplify
    val lcbp = new LCBPGeneral(FactoredScheme(p,Map(2->Set(2))))
    lcbp.calibrator.isConverged and (lcbp.logZ must beCloseTo(p.logZ,1e-9))
  } ^
  "splitting over more variables in longer chain should be close toexact" ! {
    val p = grid(30,1)
    new LCBPGeneral(FactoredScheme.withMaxDistance(Set(15),5,p)).logZ must beCloseTo(p.logZ, 1e-6)
  } ^
  "comparison of result between old and new lcbp implementation, resolved bug" ! {
    val scheme = FactoredScheme.withMaxDistance(Set(1,5),1,grid(3,6).simplify)

    val lcbp_new: LCBPGeneral = new LCBPGeneral(scheme)
    val lcbp_old: LCBP = new LCBP(scheme.problem, scheme.toGScheme, maxIterations = 100000)
    ((lcbp_new.calibrator.isConverged && lcbp_old.calibrator.isConverged) must beTrue) and
      (lcbp_new.logZ must beCloseTo(lcbp_old.logZ,1e-6))
  }
}
