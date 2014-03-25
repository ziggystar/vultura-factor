package vultura.fastfactors.algorithms

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.fastfactors._
import generators._
import scala.util.Random

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class CalibratedJunctionTreeTest extends Specification with FastFactorMatchers{
  override def is: Fragments =
    "sample marginals need to converge to true marginals" ^
      testSampleMarginals(grid(2,2,2,expGauss(1)), numSamples = 10000) ^
      testSampleMarginals(grid(3,3,2,expGauss(2)), numSamples = 10000, tol = 5e-2) ^
      testSampleMarginals(grid(1,2,2,expGauss(0.2)).toRing(LogD), numSamples = 1000, tol = 5e-2)


  def testSampleMarginals(p: Problem, numSamples: Int = 1000, tol: Double = 1e-2) = {
    val jt = new CalibratedJunctionTree(p)
    val random = new Random(0)
    val samples = IndexedSeq.fill(numSamples)(jt.sample(random))

    def empiricalMarginal(variables: Array[Int]): FastFactor = FastFactor.fromFunction(variables, p.domains,
      assignment => samples.count(sample => variables.map(sample).sameElements(assignment)) / numSamples.toDouble)

    p.factors.map(f => empiricalMarginal(f.variables) must beSimilarTo(p.ring.decode(jt.cliqueBelief(f.variables)), tol)).reduce(_ and _)
  }
}
