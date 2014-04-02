package vultura.fastfactors.estimation

import vultura.fastfactors._
import vultura.util._
import vultura.fastfactors.algorithms.JunctionTree
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.BOBYQAOptimizer

trait UnconstraintDifferentiableFunction {
  def dimension: Int
  def value(theta: IndexedSeq[Double]): Double
  def gradient(theta: IndexedSeq[Double]): IndexedSeq[Double]
  def initialGuess: IndexedSeq[Double]
}

object UnconstraintDifferentiableFunction {
  def average(fs: Seq[UnconstraintDifferentiableFunction]) = new UnconstraintDifferentiableFunction{
    override def dimension: Int = fs.head.dimension
    require(fs.forall(_.dimension == dimension), "all function must have the same dimension")

    def avg(xss: Seq[IndexedSeq[Double]]): IndexedSeq[Double] = xss.transpose.map(_.mean)(collection.breakOut)

    override def initialGuess: IndexedSeq[Double] = avg(fs.map(_.initialGuess))

    override def gradient(theta: IndexedSeq[Double]): IndexedSeq[Double] = avg(fs.map(_.gradient(theta)))

    override def value(theta: IndexedSeq[Double]): Double = fs.map(_.value(theta)).mean
  }
}

object Optimization {
  /* @return the found maximizing function values and the result. */
  def maximize(f: UnconstraintDifferentiableFunction, tol: Double = 1e-9, maxSteps: Int = 100, useGradient: Boolean = true): (IndexedSeq[Double],Double) = {
    import org.apache.commons.math3.analysis.{MultivariateVectorFunction, MultivariateFunction}
    import org.apache.commons.math3.optim._
    import nonlinear.scalar.gradient.NonLinearConjugateGradientOptimizer
    import nonlinear.scalar.{ObjectiveFunction, GoalType, ObjectiveFunctionGradient}
    import NonLinearConjugateGradientOptimizer._

    val result = if(useGradient)
      new NonLinearConjugateGradientOptimizer(
        Formula.POLAK_RIBIERE,
        new SimpleValueChecker(0, tol, maxSteps)).optimize(
          new BracketingStep(1),
          new ObjectiveFunctionGradient(new MultivariateVectorFunction {
            override def value(point: Array[Double]): Array[Double] = f.gradient(point).toArray
          }),
          new ObjectiveFunction(new MultivariateFunction {
            override def value(point: Array[Double]): Double = f.value(point)
          }),
          GoalType.MAXIMIZE,
          new InitialGuess(f.initialGuess.toArray),
          new MaxEval(maxSteps),
          new MaxIter(maxSteps)
        )
    else new BOBYQAOptimizer(f.dimension * 2 + 1).optimize(
      new ObjectiveFunction(new MultivariateFunction {
        override def value(point: Array[Double]): Double = f.value(point)
      }),
      GoalType.MAXIMIZE,
      new SimpleBounds(Array.fill(f.dimension)(Double.NegativeInfinity),Array.fill(f.dimension)(Double.PositiveInfinity)),
      new InitialGuess(f.initialGuess.toArray),
      new MaxEval(maxSteps),
      new MaxIter(maxSteps)
    )

    (result.getPoint,result.getValue)
  }
}

/** The average log-likelihood function. Supports shared parameters.
  * @param problem A markov network.
  * @param data Each entry is an observation to an instance of problem.
  * @param target Each set of features will have one shared parameter.
  */
case class CompressedAvgLogLikelihood(problem: Problem, data: Seq[Map[Var,Val]], target: IndexedSeq[Set[Feature]]) extends UnconstraintDifferentiableFunction {

  val simplifiedLogProblem: Problem = problem.simplify.toRing(LogD)

  //note that the parameters are always kept in normal representation
  type Theta = IndexedSeq[Double]

  def buildProblem(parameters: Theta): Problem = {
    val weightedFeatures: IndexedSeq[(Feature, Double)] = for{
      (features, theta) <- target zip parameters
      f <- features
    } yield (f, theta)
    simplifiedLogProblem.copy(factors = simplifiedLogProblem.factors ++ Feature.buildProblem(simplifiedLogProblem.domains, LogD, weightedFeatures).factors).simplify
  }

  def gradient(theta: Theta): IndexedSeq[Double] = {
    val p: Problem = buildProblem(theta)
    val unconditioned = new JunctionTree(p)

    val expectationMatrix = data.map{ d =>
      val conditioned = new JunctionTree(p.condition(d))
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
  def value(theta: Theta): Double = {
    val p = buildProblem(theta)
    val llMean = data.map(d => p.condition(d).logZ).mean - p.logZ
    llMean
  }

  /** @return the neutral vector (zeros in log domain there). */
  override def initialGuess: IndexedSeq[Double] = IndexedSeq.fill(dimension)(0d)

  override def dimension: Int = target.size
}

/** The log-likelihood for a single data item, but with potentially soft observations.
  * Important: The features are supposed to be in the log-domain. */
case class SingleDataSoftObsLogLikelihood(domains: Array[Int],
                                          ring: RingZ[Double],
                                          fixedFeatures: Seq[(Feature,Double)],
                                          observations: Seq[(Feature,Double)],
                                          target: IndexedSeq[Set[Feature]]) extends UnconstraintDifferentiableFunction{
  require(ring == LogD)

  val (hardObs, softObs) = observations
    .partition(_._2 == Double.PositiveInfinity)

  val observedCondition: Map[Int, Int] = hardObs
    .map(_._1.toMap)
    .foldLeft(Map[Int,Int]()){
      case (acc,nextCondition) =>
        require(nextCondition.keys.filter(acc.contains).forall(k => acc(k) == nextCondition(k)))
        acc ++ nextCondition
    }

  def valuedTarget(theta: IndexedSeq[Double]): Seq[(Feature,Double)] = for{
    (features, value) <- target zip theta
    feature <- features
  } yield (feature, value)

  def buildProblem(valuedFeatures: Seq[(Feature,Double)]): Problem = Feature.buildProblem(domains,ring,valuedFeatures)

  def unobservedProblem(theta: IndexedSeq[Double]): Problem =
    buildProblem(fixedFeatures ++ valuedTarget(theta)).simplify
  def observedProblem(theta: IndexedSeq[Double]): Problem =
    buildProblem(fixedFeatures ++ valuedTarget(theta) ++ softObs).condition(observedCondition).simplify

  override def value(theta: IndexedSeq[Double]): Double =
    observedProblem(theta).logZ - unobservedProblem(theta).logZ

  override def gradient(theta: IndexedSeq[Double]): IndexedSeq[Double] = {
    val unobserved = new JunctionTree(unobservedProblem(theta))
    val observed = new JunctionTree(observedProblem(theta))

    val featureExpectations: IndexedSeq[Double] = target.map(
      _.map(f =>
        f
          .condition(observedCondition)
          .map(conditionedFeature => math.exp(observed.cliqueBelief(conditionedFeature.variables).eval(conditionedFeature.point,domains)))
          .getOrElse(0d)
      ).mean
    )

    //average (over tied features) of the feature expectations without observations
    val parameterExpectations: IndexedSeq[Double] = target.map(
      _.map(f => math.exp(unobserved.cliqueBelief(f.variables).eval(f.point,domains))).mean
    )

    (featureExpectations zip parameterExpectations).map{case (obsExp,unobExp) => obsExp - unobExp}
  }

  override def initialGuess: IndexedSeq[Double] = IndexedSeq.fill(dimension)(0d)

  val dimension: Int = target.size
}

