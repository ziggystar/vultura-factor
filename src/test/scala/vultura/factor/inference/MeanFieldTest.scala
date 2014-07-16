package vultura.factor.inference

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.factor.{generators, Problem}
import scala.util.Random

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class MeanFieldTest extends Specification {
  val factoredProblem = generators.factorized(10,4,generators.expGauss(1),new Random(1))
  def is: Fragments =
  "mean field is exact for factorized problems" ! (new MeanField(factoredProblem).Z must beCloseTo(new JunctionTree(factoredProblem).Z,0.01))
}
