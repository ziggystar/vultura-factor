package vultura.fastfactors.estimation

import vultura.fastfactors._
import vultura.fastfactors.algorithms.CalibratedJunctionTree

/**
 * Parameter estimation task.
 * - global parameter tying
 * - fixed (input) parameters
 *
 * @param estimatees Each set corresponds to one parameter, tied for all features in the set.
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
case class LearningTask(
  domains: Array[Var],
  fixedFeatures: IndexedSeq[(Feature,Double)],
  estimatees: IndexedSeq[Set[Feature]],
  ring: RingZ[Double]){
  val numParameters = estimatees.size

  def buildProblem(theta: IndexedSeq[Double]): Problem = {
    val weightedFeatures: IndexedSeq[(Feature, Double)] =
      fixedFeatures ++ (for((fs,p) <- estimatees.zip(theta); f <- fs) yield (f,p))
    Feature.buildProblem(domains, ring, weightedFeatures)
  }
}

trait ParameterLearner {
  def task: LearningTask
  def data: Map[Var,Val]
  def estimate: (IndexedSeq[Double], Double)
}

/** Use gradient ascend and exact computation of gradients and likelihood. */
case class ExactGradientAscent(task: LearningTask, data: Map[Var,Val] = Map(), tol: Double = 1e-7, maxIter: Int = 1000) extends ParameterLearner{
  import org.apache.commons.math3.analysis.{MultivariateVectorFunction, MultivariateFunction}
  import org.apache.commons.math3.optim._
  import nonlinear.scalar.gradient.NonLinearConjugateGradientOptimizer
  import nonlinear.scalar.{ObjectiveFunction, GoalType, ObjectiveFunctionGradient}
  import NonLinearConjugateGradientOptimizer._

  //note that the parameters are always kept in normal representation
  type Theta = IndexedSeq[Double]


  def loglikelihoodGradient(theta: Theta): IndexedSeq[Double] = {
    val problem: Problem = task.buildProblem(theta)
    val unconditioned = new CalibratedJunctionTree(problem)
    val conditioned = new CalibratedJunctionTree(problem.condition(data))

    def unconditionedExpected(f: Feature): Double = unconditioned.cliqueBelief(f.variables).eval(f.point,task.domains)

    def conditionedExpected(f: Feature): Double =
      f.condition(data).map{ cFeat =>
        conditioned.cliqueBelief(cFeat.variables).eval(cFeat.point,task.domains)
      }.getOrElse(0)

    task.estimatees.map{ paramFeatures =>
      paramFeatures.map(f => conditionedExpected(f) - unconditionedExpected(f)).sum
    }
  }

  def logLikelihood(theta: Theta): Double = {
    val p = task.buildProblem(theta)
    p.condition(data).logZ - p.logZ
  }

  override val estimate: (Theta,Double) = {
    val optimizer = new NonLinearConjugateGradientOptimizer(
      Formula.POLAK_RIBIERE,
      new SimpleValueChecker(0,tol,maxIter))

    val result = optimizer.optimize(
      new BracketingStep(0.1),
      new ObjectiveFunctionGradient(new MultivariateVectorFunction {
        override def value(point: Array[Double]): Array[Double] = loglikelihoodGradient(point).toArray
      }),
      new ObjectiveFunction(new MultivariateFunction {
        override def value(point: Array[Double]): Double = logLikelihood(point)
      }),
      GoalType.MAXIMIZE,
//      new SimpleBounds(Array.fill(task.numParameters)(0),Array.fill(task.numParameters)(Double.PositiveInfinity)),
      new InitialGuess(Array.fill(task.numParameters)(1)),
      new MaxEval(maxIter),
      new MaxIter(maxIter)
    )
    (result.getPoint,result.getValue)
  }
}
