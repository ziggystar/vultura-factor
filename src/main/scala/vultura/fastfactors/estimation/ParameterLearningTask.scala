package vultura.fastfactors.estimation

import vultura.fastfactors._
import vultura.util._
import vultura.fastfactors.algorithms.CalibratedJunctionTree
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.BOBYQAOptimizer

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
  require(task.ring == LogD, "log-linear model required")

  import org.apache.commons.math3.analysis.{MultivariateVectorFunction, MultivariateFunction}
  import org.apache.commons.math3.optim._
  import nonlinear.scalar.gradient.NonLinearConjugateGradientOptimizer
  import nonlinear.scalar.{ObjectiveFunction, GoalType, ObjectiveFunctionGradient}
  import NonLinearConjugateGradientOptimizer._

  //note that the parameters are always kept in normal representation
  type Theta = IndexedSeq[Double]

  def decode(x: Double): Double = task.ring.decode(Array(x))(0)


  def loglikelihoodGradient(theta: Theta): IndexedSeq[Double] = {
    val problem: Problem = task.buildProblem(theta)
    val unconditioned = new CalibratedJunctionTree(problem)
    val conditioned = new CalibratedJunctionTree(problem.condition(data))

    def unconditionedExpected(f: Feature): Double = decode(unconditioned.cliqueBelief(f.variables).eval(f.point,task.domains))

    def conditionedExpected(f: Feature): Double =
      f.condition(data).map{ cFeat =>
        decode(conditioned.cliqueBelief(cFeat.variables).eval(cFeat.point,task.domains))
      }.getOrElse(0d)

    val gradient = task.estimatees.map(_.map(f => conditionedExpected(f) - unconditionedExpected(f)).mean)

    gradient
  }

  def logLikelihood(theta: Theta): Double = {
    val p = task.buildProblem(theta)
    val ll: Double = p.condition(data).logZ - p.logZ
    ll
  }

  override val estimate: (Theta,Double) = {
    val result = new NonLinearConjugateGradientOptimizer(
      Formula.POLAK_RIBIERE,
      new SimpleValueChecker(0, tol, maxIter)).optimize(
      new BracketingStep(0.01),
      new ObjectiveFunctionGradient(new MultivariateVectorFunction {
        override def value(point: Array[Double]): Array[Double] = loglikelihoodGradient(point).toArray
      }),
      new ObjectiveFunction(new MultivariateFunction {
        override def value(point: Array[Double]): Double = logLikelihood(point)
      }),
      GoalType.MAXIMIZE,
      new InitialGuess(Array.fill(task.numParameters)(1)),
      new MaxEval(maxIter),
      new MaxIter(maxIter)
    )
    (result.getPoint,result.getValue)
  }
}

/** Use gradient ascend and exact computation of gradients and likelihood. */
class ExactGradientAscent2(_problem: Problem, val data: Seq[Map[Var,Val]], val target: IndexedSeq[Set[Feature]], val tol: Double = 1e-7, val maxIter: Int = 1000) {
  //note that the parameters are always kept in normal representation
  type Theta = IndexedSeq[Double]

  val problem = _problem.toRing(LogD).simplify

  def buildProblem(parameters: Theta): Problem = {
    val weightedFeatures: IndexedSeq[(Feature, Double)] = for{
      (features, theta) <- target zip parameters
      f <- features
    } yield (f, theta)
    problem.copy(factors = problem.factors ++ Feature.buildProblem(problem.domains, LogD, weightedFeatures).factors).simplify
  }

  def loglikelihoodGradient(theta: Theta): IndexedSeq[Double] = {
    val p: Problem = buildProblem(theta)
    val unconditioned = new CalibratedJunctionTree(p)

    val expectationMatrix = data.map{ d =>
      val conditioned = new CalibratedJunctionTree(p.condition(d))
      //every element in target corresponds to one parameter
      target.map{ features =>
        //the average expected value over all features tied for this parameter; note that the feature might already
        //be evaluated completely by the data `d`
        features.map(f => f.condition(d).map(conditionedFeature =>
          math.exp(conditioned.cliqueBelief(conditionedFeature.variables).eval(conditionedFeature.point,p.domains))
        ).getOrElse(0d)).mean
      }
    }

    val empiricalParameterExpectations: IndexedSeq[Double] = expectationMatrix.transpose.map(_.mean)(collection.breakOut)

    //average (over tied features) of the feature expectations without observations
    val parameterExpectations: IndexedSeq[Double] = target.map{ features =>
      features.map(f => math.exp(unconditioned.cliqueBelief(f.variables).eval(f.point,p.domains))).mean
    }

    val gradient = empiricalParameterExpectations.zip(parameterExpectations).map{
      case (condExpect, uncondExpect) => condExpect - uncondExpect
    }

    gradient
  }

  /** @return The average log-likelihood of the data given the parameters `theta`. */
  def logLikelihood(theta: Theta): Double = {
    val p = buildProblem(theta)
    val llMean = data.map(d => p.condition(d).logZ).mean - p.logZ
    llMean
  }

  val estimate: (Theta,Double) = {
    import org.apache.commons.math3.analysis.{MultivariateVectorFunction, MultivariateFunction}
    import org.apache.commons.math3.optim._
    import nonlinear.scalar.gradient.NonLinearConjugateGradientOptimizer
    import nonlinear.scalar.{ObjectiveFunction, GoalType, ObjectiveFunctionGradient}
    import NonLinearConjugateGradientOptimizer._

    val useGradient = true

    val result = if(useGradient)
      new NonLinearConjugateGradientOptimizer(
        Formula.POLAK_RIBIERE,
        new SimpleValueChecker(0, tol, maxIter)).optimize(
          new BracketingStep(1),
          new ObjectiveFunctionGradient(new MultivariateVectorFunction {
            override def value(point: Array[Double]): Array[Double] = loglikelihoodGradient(point).toArray
          }),
          new ObjectiveFunction(new MultivariateFunction {
            override def value(point: Array[Double]): Double = logLikelihood(point)
          }),
          GoalType.MAXIMIZE,
          new InitialGuess(Array.fill(target.size)(0d)),
          new MaxEval(maxIter),
          new MaxIter(maxIter)
        )
    else new BOBYQAOptimizer(target.size * 2 + 1).optimize(
      //      new BracketingStep(0.1),
      //      new ObjectiveFunctionGradient(new MultivariateVectorFunction {
      //        override def value(point: Array[Double]): Array[Double] = loglikelihoodGradient(point).toArray
      //      }),
      new ObjectiveFunction(new MultivariateFunction {
        override def value(point: Array[Double]): Double = logLikelihood(point)
      }),
      GoalType.MAXIMIZE,
      new SimpleBounds(Array.fill(target.size)(Double.NegativeInfinity),Array.fill(target.size)(Double.PositiveInfinity)),
      new InitialGuess(Array.fill(target.size)(0d)),
      new MaxEval(maxIter),
      new MaxIter(maxIter)
    )

    (result.getPoint,result.getValue)
  }
}
