package vultura.fastfactors.algorithms.conditioned

import vultura.fastfactors._
import scala.collection.mutable
import vultura.fastfactors.algorithms.InfAlg

/** Just one set of variables gets conditioned. */
class SinglyLCMF(problem: Problem, scheme: SimpleScheme, tol: Double = 1e-9, maxIterations: Int = 1000) extends InfAlg {
  require(problem.ring == NormalD, "mean field only supports calculation in normal domain")
  require(problem == scheme.problem, "scheme targets a different problem")
  require(
    problem.factors.forall(f => scheme.influencesOf(f.variables.toSet).size <= 1),
    "a factor has two or more influencing conditions")

  /** The logarithmic factors of the problem adjacent to each variable. */
  val logFactors: Map[Int,IndexedSeq[FastFactor]] = {
    def logFactorsOf(v: Int): IndexedSeq[FastFactor] =
      problem.factorsOfVariable(v).map(ff => ff.copy(values = LogD.encode(ff.values)))
    problem.variables.map(v => v -> logFactorsOf(v))(collection.breakOut)
  }

  private val cWeights: mutable.Map[Int,FastFactor] = new mutable.HashMap[Int,FastFactor]()
    .withDefault(variable => FastFactor.maxEntropy(Array(variable),problem.domains,problem.ring))

  private val conditionedQs: mutable.Map[(Int,Condition),FastFactor] =
    new mutable.HashMap[(Int,Condition),FastFactor]().withDefault{ case (variable, condition) =>
      FastFactor.deterministicMaxEntropy(Array(variable),condition,problem.domains,problem.ring)
    }

  private var iterations: Int = 0

  trait Parameter{
    def effect: Set[Parameter]
  }
  case class Marginal(variable: Int, condition: Condition) extends Parameter{
    val effect: Set[Parameter] = for{
      n <- problem.neighboursOf(variable)
      cond <- scheme.conditionsOf(n) if cond.isCompatibleWith(condition)
      eff <- Seq(Marginal(n,cond),Weight(cond.head._1))
    } yield eff
  }
  case class Weight(variable: Int) extends Parameter {
    val effect: Set[Parameter] = {
      val influencedVariables = scheme.influencedVariables(variable)
      influencedVariables.flatMap(problem.neighboursOf).filterNot(influencedVariables).map(Marginal(_,Map()))
    }
  }

  private val uncalibrated: mutable.Queue[Parameter] = {
    val weightParams = scheme.splits.map(_._2).map(Weight)
    val marginalParams: Set[SinglyLCMF.this.type#Marginal] = for {
      (influencedVars, condVar) <- scheme.splits
      influenced <- influencedVars
      conditionValue <- 0 until problem.domains(condVar)
    } yield Marginal(influenced, Map(condVar -> conditionValue))

    mutable.Queue[Parameter]((marginalParams ++ weightParams).toSeq:_*)
  }


  /**
   * Forms the linear combination of the conditioned marginals.
   * @return The estimated marginal distribution over `variable` under the given condition. */
  def getMarginal(variable: Int, condition: Condition): FastFactor = {
    //there is either none or one conditioned variable having an effect on `variable`
    scheme.influencesOf(variable).toSeq match {
      //`variable` is unconditioned
      case Seq() => conditionedQs((variable,Map()))
      //make the linear combination of marginals weighted by the condition weights
      case Seq(vc) if !condition.contains(vc) =>
        val weights: Array[Double] = cWeights(vc).normalize(NormalD).values
        val conditionedMarginals: IndexedSeq[FastFactor] =
          (0 until problem.domains(vc)).map(value => conditionedQs((variable, Map(vc -> value))))
        val weightedFactors =
          weights.zip(conditionedMarginals).map{ case (weight,factor) => factor.map(_ * weight)}

        weightedFactors.reduce[FastFactor]{case (f1,f2) =>
          FastFactor(f1.variables,f1.values.zip(f2.values).map{case (v1,v2) => v1 + v2})
        }
      //a conditioned marginal is requested
      case Seq(vc) if condition.contains(vc) => conditionedQs(variable,Map(vc -> condition(vc)))
      case _ => sys.error("more than one influencing variable found")
    }
  }
  
  def createQDistribution(scope: Array[Int], condition: Condition): FastFactor = {
    //we could allow this, but let's see if it is used at all
    //it would result in a factor that includes a variable with domain different from problem.domain
    require(condition.keySet.intersect(scope.toSet).isEmpty, "trying to build distribution over conditioned variable")
    val marginals: IndexedSeq[FastFactor] = scope.map(getMarginal(_,condition))
    FastFactor.multiply(NormalD)(problem.domains)(marginals)
  }

  /** Recalculate the distribution over the conditions induced by the given variable. */
  def computeConditionWeights(variable: Int): FastFactor = {
    require(scheme.splits.exists(_._2 == variable), "argument must be a condition-variable")
    def logZOfCondition(cond: Condition): Double = {
      val entropies = scheme.influencedVariables(variable).map(v => NormalD.entropy(getMarginal(v,cond).values)).sum
      val logExpect = scheme.influencedFactors(variable).map(factor =>
        NormalD.expectation(createQDistribution(factor.variables,cond).values,factor.values.map(math.log))
      ).sum
      logExpect + entropies
    }
    val normalDistribution: Array[Double] = (
      for (c <- 0 until problem.domains(variable))
      yield math.exp(logZOfCondition(Map(variable -> c)))
    )(collection.breakOut)
    FastFactor(Array(variable),normalDistribution).normalize(NormalD)
  }

  /** Update the weights for the conditions induced by a given variable.
    * @return `true`, if the the value was changed. */
  def updateConditionWeights(variable: Int): Boolean = {
    val newWeights = computeConditionWeights(variable)
    val diff = FastFactor.maxDiff(cWeights(variable),newWeights,NormalD)
    if(diff > tol){
      cWeights.put(variable,newWeights)
      return true
    }
    false
  }

  /** Calculate the marginal distribution for a given variable and condition. No side-effects. */
  def computeMarginal(variable: Int, condition: Condition): FastFactor = {
    val factorContribs: IndexedSeq[FastFactor] = for {
      logFactor <- logFactors(variable)
      qDist = createQDistribution(logFactor.variables.filterNot(_ == variable),condition)
      expectedLog = FastFactor.multiplyRetain(NormalD)(problem.domains)(Array(logFactor, qDist), Array(variable))
    } yield expectedLog
    FastFactor.elementWiseSum(factorContribs).map(math.exp).normalize(NormalD)
  }

  /** Update the stored marginal distribution for a given variable and condition.
    * @return `true`, if the the value was changed. */
  def updateQ(variable: Int, condition: Condition): Boolean = {
    val newDist = computeMarginal(variable,condition)
    val diff = FastFactor.maxDiff(conditionedQs(variable -> condition),newDist,NormalD)
    if(diff > tol){
      conditionedQs.put(variable -> condition,newDist)
      return true
    }
    false
  }

  /** Calibrates the problem with a round-robin schedule according to variable indices.
    * @return `true` if calibration was achieved. */
  def calibrate(cTol: Double, cMaxIter: Int): Boolean = {
    def updateParameter(p: Parameter): Seq[Parameter] = if(p match {
      case Weight(vc) => updateConditionWeights(vc)
      case Marginal(v,c) => updateQ(v,c)
    }) p.effect.toSeq else Seq()

    while(iterations < cMaxIter && !uncalibrated.isEmpty){
      val nextParam = uncalibrated.dequeue()
      val touched = updateParameter(nextParam)
      uncalibrated.enqueue(touched.filterNot(uncalibrated.contains).toSeq:_*)
      iterations += 1
    }
    !uncalibrated.isEmpty
  }
}