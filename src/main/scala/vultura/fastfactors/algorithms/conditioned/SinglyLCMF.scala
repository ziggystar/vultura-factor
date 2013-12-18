package vultura.fastfactors.algorithms.conditioned

import vultura.fastfactors._
import scala.collection.mutable
import vultura.fastfactors.algorithms.conditioned.SimpleScheme

/** Just one set of variables gets conditioned. */
class SinglyLCMF(problem: Problem, scheme: SimpleScheme, tol: Double = 1e-9, maxIterations: Int = 1000){
  require(problem.ring == NormalD, "mean field only supports calculation in normal domain")
  require(problem == scheme.problem, "scheme targets a different problem")
  require(
    problem.factors.forall(f => scheme.influencesOf(f.variables.toSet).size <= 1),
    "a factor has two or more influencing conditions")

  /** The logarithmic factors of the problem adjacent to each variable. */
  val logFactors: Map[Int,IndexedSeq[FastFactor]] =
    problem.variables.map(v => v -> problem.factorsOfVariable(v).map(ff => ff.copy(values=LogD.encode(ff.values))))(collection.breakOut)  

  private val cWeights: mutable.Map[Int,FastFactor] = new mutable.HashMap[Int,FastFactor]()
    .withDefault(variable => FastFactor.maxEntropy(Array(variable),problem.domains,problem.ring))

  private val conditionedQs: mutable.Map[(Int,Condition),FastFactor] =
    new mutable.HashMap[(Int,Condition),FastFactor]().withDefault{
      case (variable, condition) => FastFactor.deterministicMaxEntropy(Array(variable),condition,problem.domains,problem.ring)
    }

  private var iterations: Int = 0

  def getQ(variable: Int, condition: Condition): FastFactor =
    conditionedQs((variable,condition.limit(scheme.influencesOf(variable))))
  
  def createQDistribution(scope: Array[Int], condition: Condition): FastFactor = ???

  /** Recalculate the distribution over the conditions induced by the given variable. */
  def recalcConditionWeights(variable: Int): FastFactor = {
    require(scheme.splits.exists(_._2 == variable), "argument must be a condition-variable")
    def logZOfCondition(cond: Condition): Double = {
      val entropies = scheme.influencedVariables(variable).map(v => NormalD.entropy(getQ(v,cond).values)).sum
      val logexpect = scheme.influencedFactors(variable).map(factor =>
        NormalD.expectation(createQDistribution(factor.variables,cond).values,factor.values.map(math.log))
      ).sum
      entropies + logexpect
    }
    val normalDistribution: Array[Double] =
      (for (c <- 0 until problem.domains(variable)) yield math.exp(logZOfCondition(Map(variable -> c))))(collection.breakOut)
    FastFactor(Array(variable),normalDistribution).normalize(LogD)
  }

  def updateConditionWeights(variable: Int): Boolean = {
    val newWeights = recalcConditionWeights(variable)
    val diff = FastFactor.maxDiff(cWeights(variable),newWeights,NormalD)
    if(diff > tol){
      cWeights.put(variable,newWeights)
      return true
    }
    false
  }

  def recalcQ(variable: Int, condition: Condition): FastFactor = {
    val factorContribs: IndexedSeq[FastFactor] = for {
      logFactor <- logFactors(variable)
      qdist = createQDistribution(logFactor.variables.filterNot(_ == variable),condition)
      expectedLog = FastFactor.multiplyRetain(NormalD)(problem.domains)(Array(logFactor, qdist), Array(variable))
    } yield expectedLog
    FastFactor.elementWiseSum(factorContribs).map(math.exp).normalize(NormalD)
  }

  def updateQ(variable: Int, condition: Condition): Boolean = {
    val newDist = recalcQ(variable,condition)
    val diff = FastFactor.maxDiff(conditionedQs(variable -> condition),newDist,NormalD)
    if(diff > tol){
      conditionedQs.put(variable -> condition,newDist)
      return true
    }
    false
  }



}