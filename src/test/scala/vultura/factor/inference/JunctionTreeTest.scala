package vultura.factor.inference

import org.specs2.Specification
import vultura.factor._
import generators._
import scala.util.Random

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class JunctionTreeTest extends Specification with FactorMatchers {
  val p1 = grid(2,2,2,expGauss(1))
  def shuffle[A](xs: Seq[A], seed: Long = 0) = new Random(seed).shuffle(xs)

  override def is = s2"" ^
    "sample marginals need to converge to true marginals" ^
      "grid 2x2" ! testSampleMarginals(grid(2,2,2,expGauss(1)), numSamples = 10000) ^
      "grid 3x3" ! testSampleMarginals(grid(3,3,2,expGauss(2)), numSamples = 10000, tol = 5e-2) ^
      "log ring sampling" ! testSampleMarginals(grid(1,2,2,expGauss(0.2)).toRing(LogD), numSamples = 1000, tol = 5e-2) ^
    p^
    "all orders must produce same marginals" !
      (new JunctionTree(p1).logZ must beCloseTo(new JunctionTree(p1, RandomOrderer()).logZ, 1e-9)) ^
    "must throw when given order misses variables" !
      (new JunctionTree(p1,VariableOrderer.fromOrder(p1.variables.tail)) must throwA[Exception]) ^
    "must throw when given intractable problem" ! {
      new JunctionTree(grid(20,20,20)) must throwA[RuntimeException]("size overflow during junction tree construction: found tree decomposition is too large")
    } ^
  "test on small problems vs exact enumeration" ^
  "2x2 grid" ! (new JunctionTree(p1) must haveSameLogZ(ExactByEnumeration(p1),1e-12))

  def testSampleMarginals(p: Problem, numSamples: Int = 1000, tol: Double = 1e-2) = {
    val jt = new JunctionTree(p)
    val random = new Random(0)
    val samples = IndexedSeq.fill(numSamples)(jt.sample(random))

    def empiricalMarginal(variables: Array[Int]): Factor = Factor.fromFunction(variables, p.domains,
      assignment => samples.count(sample => variables.map(sample).sameElements(assignment)) / numSamples.toDouble)

    p.factors.map(f => empiricalMarginal(f.variables) must beSimilarTo(p.ring.decode(jt.cliqueBelief(f.variables)), tol)).reduce(_ and _)
  }
}
