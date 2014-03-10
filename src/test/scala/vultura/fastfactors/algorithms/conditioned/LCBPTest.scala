package vultura.fastfactors.algorithms.conditioned

import org.specs2.Specification
import org.specs2.specification.Fragments

import vultura.fastfactors.generators._
import scala.util.Random
import vultura.fastfactors.algorithms.{CalibratedJunctionTree, BeliefPropagation}
import java.io.{PrintStream, FileOutputStream}
import scala.sys.process._
import vultura.fastfactors.algorithms.calibration.BP_Cal
import vultura.fastfactors.Problem

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class LCBPTest extends Specification {
  val p1 = grid(2,2,2,expGauss(0.5))
  def fullyConditioned(p: Problem, cv: Int) = GScheme(p.variables.map(v => v -> LScheme.split(cv,p.domains)).toMap)
  val p2 = grid(4,4,2,expGauss(1))
  def slightlyConditioned(p: Problem, cv: Int) = GScheme((p.neighboursOf(cv) + cv).map(v => v -> LScheme.split(cv,p.domains)).toMap)

  override def is: Fragments =
    "unconditioned" ^
      "yield same result as BP" ^
        "on simple loop" ! (new LCBP(p1,GScheme(),1e-7,10000).logZ must beCloseTo(new BeliefPropagation(p1,new Random(0),1e-7,10000).logZ,0.0001)) ^
        "on less simple loop" ! (new LCBP(p2,GScheme(),1e-7,10000).logZ must beCloseTo(new BeliefPropagation(p2,new Random(0),1e-7,10000).logZ,0.0001)) ^
      p^
    p^
    "fully conditioned" ^
      {
        new LCBP(p1,fullyConditioned(p1,0)).logZ must beCloseTo(CalibratedJunctionTree.logZ(p1),1e-5)
      } ^
    p^
    "locally conditoned" ^
      {
        new LCBP(p2,slightlyConditioned(p2,0)).logZ must beCloseTo(CalibratedJunctionTree.logZ(p2),1e-5)
      }


}
