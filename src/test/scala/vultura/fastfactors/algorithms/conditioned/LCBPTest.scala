package vultura.fastfactors.algorithms.conditioned

import org.specs2.Specification
import org.specs2.specification.Fragments

import vultura.fastfactors.generators._
import scala.util.Random
import vultura.fastfactors.algorithms.BeliefPropagation

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class LCBPTest extends Specification {
  val p1 = grid(4,4,2,expGauss(0.5))
  val p2 = grid(4,4,2,expGauss(0.5))
  override def is: Fragments =
    (new LCBP(p1,GScheme(),1e-7,10000).logZ must beCloseTo(new BeliefPropagation(p1,new Random(0),1e-7,10000).logZ,0.0001))
}
