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
  val problem4x4: Problem = grid(2,2)
  def lcbp4x4_pointless = new LCBPGeneral(FactoredScheme(problem4x4.simplify, Map(0 -> Set(0))))
  def lcbp4x4_empty = new LCBPGeneral(FactoredScheme(problem4x4.simplify, Map()))
  //this breaks the loop
  def lcbp4x4_jt_exact = new LCBPGeneral(FactoredScheme(problem4x4.simplify, Map(0 -> Set(0), 1 -> Set(0), 2 -> Set(0), 3 -> Set(0))))
  def lcbp4x4_jt_exact2 = new LCBPGeneral(FactoredScheme(problem4x4.simplify, Map(0 -> Set(2), 1 -> Set(2), 2 -> Set(2), 3 -> Set(2))))

  val tree: Problem = treeK(5,2)

  def lcbp4x4_3cond = FactoredScheme.withMaxDistance(Set(1,5),1,grid(3,6).simplify)

  override def is: Fragments =
  "pointless scheme" ! (lcbp4x4_pointless.logZ must beCloseTo(LBP.infer(problem4x4,tol = 1e-9).logZ,1e-2)) ^
  "empty scheme is same as BP" ! (lcbp4x4_empty.logZ must beCloseTo(LBP.infer(problem4x4,tol = 1e-9).logZ,1e-6)) ^
  "exact" ! (lcbp4x4_jt_exact.logZ must beCloseTo(problem4x4.logZ,1e-6)) ^
  " with different conditioner" ! (lcbp4x4_jt_exact2.logZ must beCloseTo(problem4x4.logZ,1e-6)) ^
  "splitting on a tree should remain exact" !
    {
      new LCBPGeneral(FactoredScheme(tree)).calibrator.toDot.toPDF("empty.pdf")
      new LCBPGeneral(FactoredScheme.withMaxDistance(Set(0),0,tree)).calibrator.toDot.toPDF("md0.pdf")
      new LCBPGeneral(FactoredScheme.withMaxDistance(Set(0),1,tree)).calibrator.toDot.toPDF("md1.pdf")
      (new LCBPGeneral(FactoredScheme.withMaxDistance(Set(0),1,tree)).logZ must beCloseTo(tree.logZ, 1e-6))
    } ^
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
    lcbp.calibrator.toDot.toPDF("4chain-lcbp.pdf")
    new LCBPGeneral(FactoredScheme(p)).calibrator.toDot.toPDF("4chain-uncond.pdf")
    lcbp.calibrator.isConverged and (lcbp.logZ must beCloseTo(p.logZ,1e-9))
  } ^
  "splitting over more variables in longer chain should be exact" ! {
    val p = grid(30,1)
    new LCBPGeneral(FactoredScheme.withMaxDistance(Set(15),5,p)).logZ must beCloseTo(p.logZ, 1e-6)
  } ^
  "conditioning three variables on 4x4 should be not extremely off" ! {
    val lcbp_new: LCBPGeneral = new LCBPGeneral(lcbp4x4_3cond)
    val lcbp_old: LCBP = new LCBP(lcbp4x4_3cond.problem, lcbp4x4_3cond.toGScheme, maxIterations = 100000)
    println(lcbp_new.calibrator.toCSV)
    println(lcbp_old.calibrator.toCSV)
    ((lcbp_new.calibrator.isConverged && lcbp_old.calibrator.isConverged) must beTrue) and
      (lcbp_new.logZ must beCloseTo(lcbp_old.logZ,1e-6))
  }
}
