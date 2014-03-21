package vultura.fastfactors.estimation

import vultura.fastfactors._
import org.apache.commons.math3.optim.nonlinear.scalar.gradient.NonLinearConjugateGradientOptimizer
import org.apache.commons.math3.optim._
import org.apache.commons.math3.optim.nonlinear.scalar.{ObjectiveFunction, GoalType, ObjectiveFunctionGradient}
import org.apache.commons.math3.analysis.{MultivariateVectorFunction, MultivariateFunction}
import vultura.fastfactors.algorithms.CalibratedJunctionTree
import scala.util.hashing.MurmurHash3

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
}

case class Feature(variables: Array[Var], point: Array[Val]) {
  /**
    * @param domains Maps variables to their domain size.
    * @param ring Ring used by resulting factor.
    * @param theta Must be in normal domain.
    * @return A FastFactor yielding `theta` for assignments in `points`, and one otherwise.
    */
  def toFastFactor(domains: Array[Int], ring: RingZ[Double], theta: Double) =
   FastFactor(variables,Array.fill(variables.map(domains).product)(ring.one)).set(point,domains,theta)

  /** Removes all conditioned variables from the features scope and also removes all contradicted points. */
  def condition(c: Map[Var,Val]): Option[Feature] = {
    val compatibleVals = variables.zip(point).count{ case (variable, value) => c.get(variable).forall(_ == value) }
    if(compatibleVals == point.size){
      val (newVars, newVals) = variables.zip(point).filterNot(ass => c.contains(ass._1)).unzip
      Some(Feature(newVars.toArray,newVals.toArray))
    }
    else
      None
  }

  lazy val memoHash = (MurmurHash3.arrayHash(variables), MurmurHash3.arrayHash(point)).hashCode
  override def hashCode(): Int = memoHash

  override def equals(obj: scala.Any): Boolean = obj match {
    case f@Feature(v,p) =>
      f.hashCode() == this.hashCode &&
      f.variables.deep == this.variables.deep &&
      f.point.deep == this.point.deep
    case _ => false
  }

  override def toString: String =
    s"Feature(VS(${variables.mkString(",")}), Points(${point.mkString(",")}))"
}

object Feature {
  /** Features must be in normal domain. */
  def buildProblem(domains: Array[Int], ring: RingZ[Double], weightedFeatures: Iterable[(Feature,Double)]): Problem =
    Problem(weightedFeatures.map{case(f,t) => f.toFastFactor(domains,ring,t)}(collection.breakOut), domains, ring)
}

trait ParameterLearner{
  def estimate: (IndexedSeq[Double], Double)
}

/** Use gradient ascend and exact computation of gradients and likelihood. */
case class ExactGradientAscent(task: LearningTask, data: Map[Var,Val], tol: Double = 1e-7, maxIter: Int = 1000) extends ParameterLearner{
  import NonLinearConjugateGradientOptimizer._

  //note that the parameters are always kept in normal representation
  type Theta = IndexedSeq[Double]

  def buildProblem(theta: Theta): Problem = {
    val weightedFeatures: IndexedSeq[(Feature, Double)] =
      task.fixedFeatures ++ (for((fs,p) <- task.estimatees.zip(theta); f <- fs) yield (f,p))
    Feature.buildProblem(task.domains, task.ring, weightedFeatures)
  }

  def loglikelihoodGradient(theta: Theta): IndexedSeq[Double] = {
    val problem: Problem = buildProblem(theta)
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
    val p = buildProblem(theta)
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
      new SimpleBounds(Array.fill(task.numParameters)(0),Array.fill(task.numParameters)(Double.PositiveInfinity)),
      new InitialGuess(Array.fill(task.numParameters)(1)),
      new MaxEval(maxIter),
      new MaxIter(maxIter)
    )
    (result.getPoint,result.getValue)
  }
}






