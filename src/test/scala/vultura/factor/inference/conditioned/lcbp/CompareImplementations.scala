package vultura.factor.inference.conditioned.lcbp

import org.specs2._
import org.specs2.specification.Fragments
import vultura.factor.FactorMatchers
import vultura.factor.generators._

import scala.util.Random

/**
 * Created by thomas on 17.02.15.
 */
class CompareImplementations extends Specification with FactorMatchers {
  val tree = treeK(10,3,random = new Random(0))
  val treeSplit1 = FactoredScheme.withMaxDistance(Set(5),2,tree)
  override def is: Fragments =
    (OldLCBP.inferWithScheme(treeSplit1)._1 must haveSameMarginals(LCBP_G_Exact.inferWithScheme(treeSplit1)._1,1e-7))
}
