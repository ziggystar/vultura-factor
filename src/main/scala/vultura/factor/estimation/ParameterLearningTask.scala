package vultura.factor.estimation

import java.util.logging.Logger

import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.BOBYQAOptimizer
import vultura.factor._
import vultura.factor.inference.{JointMargI, JunctionTree, ParFunI}
import vultura.util._
import vultura.util.stats._

import scala.collection.mutable
import scala.language.reflectiveCalls

trait UnconstraintDifferentiableFunction { outer =>
  def dimension: Int
  def value(theta: IndexedSeq[Double]): Double
  def gradient(theta: IndexedSeq[Double]): IndexedSeq[Double]
  def initialGuess: IndexedSeq[Double]

  def withLogging(logger: Logger): UnconstraintDifferentiableFunction  = new UnconstraintDifferentiableFunction{
    override def initialGuess: IndexedSeq[Double] = {
      val guess = outer.initialGuess
      logger.fine(s"initial guess: ${pretty(guess)}")
      guess
    }
    override def gradient(theta: IndexedSeq[Double]): IndexedSeq[Double] = {
      val (gr,cpu) = Benchmark.benchmarkCPUTime(outer.gradient(theta))
      logger.finer(s"gradient: ${pretty(gr)} @ ${pretty(theta)}; in ${cpu*1e-9}s CPU")
      gr
    }
    override def value(theta: IndexedSeq[Double]): Double = {
      val (v,cpu) = Benchmark.benchmarkCPUTime(outer.value(theta))
      logger.fine(s"value: $v @ ${pretty(theta)}; in ${cpu*1e-9}s CPU")
      v
    }
    override def dimension: Int = outer.dimension
    def pretty(xs: IndexedSeq[Double]): String = s"[${xs.map("%.5f".format(_)).mkString(" ")}]"
  }

  def setGuess(guess: IndexedSeq[Double]): UnconstraintDifferentiableFunction = new UnconstraintDifferentiableFunction {
    require(guess.size == outer.dimension, "initial guess must have dimensionality of function")
    override def initialGuess: IndexedSeq[Double] = guess

    override def dimension: Int = outer.dimension
    override def value(theta: IndexedSeq[Double]): Double = outer.value(theta)
    override def gradient(theta: IndexedSeq[Double]): IndexedSeq[Double] = outer.gradient(theta)
  }
}

object UnconstraintDifferentiableFunction {
  def average(fs: Seq[UnconstraintDifferentiableFunction]): UnconstraintDifferentiableFunction  =
    new UnconstraintDifferentiableFunction{
      override def dimension: Int = fs.head.dimension
      require(fs.forall(_.dimension == dimension), "all function must have the same dimension")

      def avg(xss: Seq[IndexedSeq[Double]]): IndexedSeq[Double] = xss.transpose.map(_.mean)(collection.breakOut)

      override def initialGuess: IndexedSeq[Double] = avg(fs.map(_.initialGuess))

      override def gradient(theta: IndexedSeq[Double]): IndexedSeq[Double] = {
        val result = avg(fs.par.map(_.gradient(theta)).seq)
        result
      }

      override def value(theta: IndexedSeq[Double]): Double = {
        val result = fs.par.map(_.value(theta)).seq.mean
        result
      }
    }
}

object Optimization {
  /* @return the found maximizing function values and the result. */
  def maximize(f: UnconstraintDifferentiableFunction, tol: Double = 1e-5, maxSteps: Int = 100, useGradient: Boolean = true, bracketingStep: Double = 3d): (IndexedSeq[Double],Double) = {
    import org.apache.commons.math3.analysis.{MultivariateFunction, MultivariateVectorFunction}
    import org.apache.commons.math3.optim._
    import org.apache.commons.math3.optim.nonlinear.scalar.gradient.NonLinearConjugateGradientOptimizer
    import org.apache.commons.math3.optim.nonlinear.scalar.gradient.NonLinearConjugateGradientOptimizer._
    import org.apache.commons.math3.optim.nonlinear.scalar.{GoalType, ObjectiveFunction, ObjectiveFunctionGradient}

    val result = if(useGradient)
      new NonLinearConjugateGradientOptimizer(
        Formula.POLAK_RIBIERE,
        new SimpleValueChecker(tol, tol, maxSteps)).optimize(
          new BracketingStep(bracketingStep),
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

/** The average log-likelihood function. Supports shared parameters. "MObs" means multiple observation,
  * means data contains observations to the same model instance. This is in contrast to e.g. training on
  * temporal sequences of differing length.
  * 
  * @param problem A markov network.
  * @param data Each entry is an observation to an instance of the given problem.
  * @param target Each set of features will have one shared parameter.
  */
case class MObsAvgLogLikelihood(problem: Problem,
                                data: Seq[Map[Var,Val]], 
                                target: IndexedSeq[Seq[Feature]],
                                inferer: Problem => JointMargI with ParFunI = new JunctionTree(_)) extends UnconstraintDifferentiableFunction {
  require(target.flatten.toSet.size == target.map(_.size).sum, "all features must be distinct")
  require(target.flatten.forall(f => f.variables.deep == f.variables.sorted.deep), "feature variable must be sorted")

  def hashMemo[A,B](f: A => B): A => B = {
    val hash = new mutable.HashMap[A,B]()
    a: A => hash.getOrElseUpdate(a,f(a))
  }

  //note that the parameters are always kept in normal representation
  type Theta = IndexedSeq[Double]

  /** the feature pointers are tuples for each feature. First is index into featureFactors array, second is index into
    * data array of the factor. */
  val (featureFactors: IndexedSeq[Factor], featurePointers: IndexedSeq[Seq[(Int, Int)]]) = {
    val scopes: SIIndex[IndexedSeq[Var]] = new SIIndex(target.flatten.map(_.variables.toIndexedSeq))
    val factors: IndexedSeq[Factor] = scopes.elements.map(vars => Factor.fromFunction(vars.toArray,problem.domains,_ => LogD.one))
    val pointers = target.map(featureChunk =>
      featureChunk.map{ feature =>
        val fi = scopes.forward(feature.variables.toIndexedSeq)
        val index = factors(fi).index(feature.point,problem.domains)
        (fi,index)
      }
    )
    (factors,pointers)
  }

  //to make quick copies, the feature factors are prefixed before the factors of the problem
  val simplifiedLogProblem: Problem = problem.simplify.toRing(LogD)

  val threadLocalProblem = new ThreadLocal[Problem]{
    override def initialValue(): Problem = simplifiedLogProblem.copy(factors = featureFactors ++ simplifiedLogProblem.factors)
  }

  def buildProblem(parameters: Theta): Problem = {
    val myCopy = threadLocalProblem.get
    for{
      (featureChunk,param) <- featurePointers zip parameters
      (fi,di) <- featureChunk
    } {
      myCopy.factors(fi).values(di) = param
    }
    myCopy
  }

  def gradient(theta: Theta): IndexedSeq[Double] = {
    val p: Problem = buildProblem(theta)
    val unconditioned = inferer(p)

    val unconditionedCliqueBeliefs = hashMemo[IndexedSeq[Int],Factor](vars => unconditioned.decodedCliqueBelief(vars.toArray))

    val expectationMatrix = data.map{ d =>
      val conditioned = inferer(p.condition(d))
      val conditionedCliqueBeliefs = hashMemo[IndexedSeq[Int],Factor](vars => conditioned.decodedCliqueBelief(vars.toArray))
      //every element in target corresponds to one parameter
      target.map{ features =>
      //the average expected value over all features tied for this parameter; note that the feature might already
      //be evaluated completely by the data `d`
        features.toIndexedSeq.map(f =>
          f.condition(d).map{
            case cf if cf.variables.isEmpty => 1d
            case cf => conditionedCliqueBeliefs(cf.variables).eval(cf.point,p.domains)
          }.getOrElse(0d)
        ).mean
      }
    }

    val empiricalParameterExpectations: IndexedSeq[Double] = expectationMatrix.transpose.map(_.mean)(collection.breakOut)

    //average (over tied features) of the feature expectations without observations
    val parameterExpectations: IndexedSeq[Double] = target.map{ features =>
      features.map(f => unconditionedCliqueBeliefs(f.variables).eval(f.point,p.domains)).mean
    }

    val gradient = empiricalParameterExpectations.zip(parameterExpectations).map{
      case (condExpect, uncondExpect) => condExpect - uncondExpect
    }
    gradient
  }

  /** @return The average log-likelihood of the data given the parameters `theta`. */
  def value(theta: Theta): Double = {
    val p = buildProblem(theta)
    val llMean = data.map(d => inferer(p.condition(d)).logZ).mean - inferer(p).logZ
    llMean
  }

  /** @return the neutral vector (zeros in log domain there). */
  override def initialGuess: IndexedSeq[Double] = IndexedSeq.fill(dimension)(0d)

  override def dimension: Int = target.size
}

/** The log-likelihood for a single data item, but with potentially soft observations.
  * Important: The features are supposed to be in the log-domain. */
case class SingleDataSoftObsLogLikelihood(domains: Array[Int],
                                          ring: Ring[Double],
                                          fixedFeatures: Seq[(Feature,Double)],
                                          observations: Seq[(Feature,Double)],
                                          target: IndexedSeq[Seq[Feature]],
                                          inferer: Problem => JointMargI with ParFunI = new JunctionTree(_)) extends UnconstraintDifferentiableFunction{
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
    inferer(observedProblem(theta)).logZ - inferer(unobservedProblem(theta)).logZ

  override def gradient(theta: IndexedSeq[Double]): IndexedSeq[Double] = {
    val unobserved = inferer(unobservedProblem(theta))
    val observed = inferer(observedProblem(theta))

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

