package vultura.fastfactors.estimation

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.fastfactors._
import vultura.fastfactors.generators._
import scala.Some
import org.specs2.matcher.MatchResult

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class FeatureTest extends Specification with FastFactorMatchers {
  val simpleGrid = grid(6,6,2,expGauss(1))
  val simpleGridLog = simpleGrid.toRing(LogD)
  val clauseGrid = grid(6,6,2,sigmaClause(1d))
  val clauseGridLog = clauseGrid.toRing(LogD)

  val domain4 = Array(2,2,2,2)

  override def is: Fragments =
    "condition feature" ^
      (Feature(Array(0,1),Array(0,0)).condition(Map(0->0)) === Some(Feature(Array(1), Array(0)))) ^
      (Feature(Array(0,1),Array(0,1)).condition(Map(0->0)) === Some(Feature(Array(1), Array(1)))) ^
      (Feature(Array(0,1),Array(0,1)).condition(Map(1->0)) === None) ^
    p^
    "feature to factor" ^
      (Feature(Array(0,1),Array(0,0)).toFastFactor(Array(2,2),NormalD,2) === FastFactor(Array(0,1),Array(2,1,1,1))) ^
    p^
    "rescaling problems" ^
      "simplegrid" ! (Feature.streamLineProblem(simpleGrid).logZ must beCloseTo(simpleGrid.logZ, 1e-9)) ^
      "simplegrid (log)" ! (Feature.streamLineProblem(simpleGridLog).logZ must beCloseTo(simpleGridLog.logZ, 1e-9)) ^
      "clause grid" ! (Feature.streamLineProblem(clauseGrid).logZ must beCloseTo(clauseGrid.logZ, 1e-9)) ^
      "clause grid (log)" ! (Feature.streamLineProblem(clauseGridLog).logZ must beCloseTo(clauseGridLog.logZ, 1e-9)) ^
    p^
    "to feature and back" ^
      toFeatureAndBack(simpleGrid) ^
      toFeatureAndBack(simpleGridLog) ^
      toFeatureAndBack(clauseGrid) ^
      toFeatureAndBack(clauseGridLog)

  def toFeatureAndBack(p: Problem): MatchResult[Double] =
    Feature.buildProblem(p.domains, p.ring, Feature.extractFeaturesFromProblem(p)).logZ must beCloseTo(p.logZ, 1e-9)

}
