package vultura.fastfactors.estimation

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.fastfactors._
import generators._
import scala.util.Random
import vultura.fastfactors.algorithms.CalibratedJunctionTree
import org.specs2.matcher.MatchResult

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class packageTest extends Specification with FastFactorMatchers {
  val maxEntropyTwoVarProblem = Problem(IndexedSeq(FastFactor(Array(0,1),Array.fill(4)(1d))),Array(2,2),NormalD).toRing(LogD)
  //delete two factor values
  val meProblemParamdel = Problem(IndexedSeq(FastFactor(Array(0,1),Array(1d,Double.NaN,Double.NaN,1d))),Array(2,2),NormalD).toRing(LogD)
  val meProblemData: Seq[Map[Var,Val]] = Seq(Map(0->0,1->0),Map(0->1,1->0),Map(0->0,1->1),Map(0->1,1->1))

  val mleLearningTask: LearningTask = LearningTask(
    Array(2,2),
    IndexedSeq(
      Feature(Array(0,1),Array(0,0)) -> 0d,
      Feature(Array(0,1),Array(1,1)) -> 0d
    ),
    IndexedSeq(
      Set(Feature(Array(0,1),Array(0,1))),
      Set(Feature(Array(0,1),Array(1,0)))
    ),
    LogD
  )

  override def is: Fragments =
    "learn two var maximum entropy model" !
      (mleEstimate(meProblemParamdel,meProblemData)._1.factors.head must beSimilarTo(maxEntropyTwoVarProblem.factors.head)) ^
    "learning on random problems" ^
      "1x3 with one missing parameter" ! randomCompletelyObservedLearningTest(grid(1,3,2,expGauss(0.3)), 1, 50) ^
      "3x3 with one missing parameter" ! randomCompletelyObservedLearningTest(grid(4,4,2,expGauss(1)), 1, 20) ^
      randomCompletelyObservedLearningTest(grid(3,3,2,expGauss(1)), 2, 50) ^
      randomCompletelyObservedLearningTest(grid(3,3,2,expGauss(1)), 3, 50)

  def randomCompletelyObservedLearningTest(_problem: Problem, numParameters: Int, numData: Int = 50): MatchResult[IndexedSeq[Double]] = {
    val random = new Random(0)
    val problem = _problem.simplify.toRing(LogD)
    val jt = new CalibratedJunctionTree(problem)
    val data = Seq.fill(numData)(jt.sample(random))

    val (droppedFeatures, keptFeatures) = random.shuffle(Feature.extractFeaturesFromProblem(problem).toIndexedSeq).splitAt(numParameters)

    require(droppedFeatures.map(_._1).forall{ feature =>
      val frequency: Double = data.count(d => feature.condition(d).isDefined) / numData.toDouble
      frequency > 0.1 && frequency < 0.9
    }, "unbalanced feature selected for training")

    val gradientAscend = new ExactGradientAscent2(Feature.buildProblem(problem.domains,problem.ring,keptFeatures),data,droppedFeatures.map(fv => Set(fv._1)))

    val (estimate,_) = gradientAscend.estimate

    println(s"estimated\n${gradientAscend.buildProblem(estimate).map(_.normalize(LogD))}")
    println(s"original\n${problem.map(_.normalize(LogD))}")

    estimate must beCloseTo(droppedFeatures.map(_._2))
  }
}
