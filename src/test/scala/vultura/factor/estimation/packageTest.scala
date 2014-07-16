package vultura.factor.estimation

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.factor._
import generators._
import scala.util.Random
import vultura.factor.inference.JunctionTree
import org.specs2.matcher.MatchResult

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class packageTest extends Specification with FastFactorMatchers {

  override def is: Fragments =
    "learning on random problems, completely observed" ^
      "1x3 with two missing parameters" ! randomCompletelyObservedLearningTest(grid(1,2,2,expGauss(0.1)), 2, 2000).orSkip ^
      "4x4 with one missing parameter" ! randomCompletelyObservedLearningTest(grid(4,4,2,expGauss(0.1)), 1, 2000).orSkip ^
      randomCompletelyObservedLearningTest(grid(3,3,2,expGauss(0.1)), 2, 1000).orSkip ^
      randomCompletelyObservedLearningTest(grid(3,3,2,expGauss(0.1)), 3, 1000).orSkip

  def randomCompletelyObservedLearningTest(_problem: Problem, numParameters: Int, numData: Int = 50): MatchResult[IndexedSeq[Double]] = {
    val random = new Random(1)
    val problem = _problem.simplify.toRing(LogD)
    val jt = new JunctionTree(problem)
    val data = Seq.fill(numData)(jt.sample(random))

    val (droppedFeatures, keptFeatures) = random.shuffle(Feature.extractFeaturesFromProblem(problem).toIndexedSeq).splitAt(numParameters)

    require(droppedFeatures.map(_._1).forall{ feature =>
      val frequency: Double = data.count(d => feature.condition(d).isDefined) / numData.toDouble
      frequency > 0.1 && frequency < 0.9
    }, "unbalanced feature selected for training")

    val target = MObsAvgLogLikelihood(Feature.buildProblem(problem.domains,problem.ring,keptFeatures), data, droppedFeatures.map(fv => Seq(fv._1)))
    val (optimized, _) = Optimization.maximize(target)

    optimized must beCloseTo(droppedFeatures.map(_._2))
  }
}
