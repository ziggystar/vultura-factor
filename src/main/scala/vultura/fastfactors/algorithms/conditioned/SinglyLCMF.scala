package vultura.fastfactors.algorithms.conditioned

import vultura.fastfactors._
import scala.collection.mutable
import vultura.fastfactors.algorithms.InfAlg
import vultura.util.IntDomainCPI

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

  //the normalized distribution over the conditioning variables
  private val cWeights: mutable.Map[Int,FastFactor] = new mutable.HashMap[Int,FastFactor]()
    .withDefault(variable => FastFactor.maxEntropy(Array(variable),problem.domains,problem.ring))

  private val conditionedQs: mutable.Map[(Int,Condition),FastFactor] =
    new mutable.HashMap[(Int,Condition),FastFactor]().withDefault((createInitialMarginal(_,_)).tupled)

  /** Build the initial marginal belief for a given variable and condition .*/
  def createInitialMarginal(variable: Int, condition: Condition): FastFactor = {
    require(condition.keySet == scheme.influencesOf(variable))
    FastFactor.deterministicMaxEntropy(Array(variable), condition, problem.domains, problem.ring)
  }

  private var iterations: Int = 0

  sealed trait Parameter{ def effect: Set[Parameter]}
  case class Marginal(variable: Int, condition: Condition) extends Parameter{
    def effect: Set[Parameter] = for{
      n <- problem.neighboursOf(variable)
      cond <- scheme.conditionsOf(n) if cond.isCompatibleWith(condition)
      eff <- Seq(Marginal(n,cond)) ++ cond.headOption.map{case (cv,_) => Seq(Weight(cv))}.getOrElse(Seq())
    } yield eff
  }
  case class Weight(variable: Int) extends Parameter {
    def effect: Set[Parameter] = {
      val influencedVariables = scheme.influencedVariables(variable)
      influencedVariables.flatMap(problem.neighboursOf).filterNot(influencedVariables).map(Marginal(_,Map()))
    }
  }

  private val uncalibrated: mutable.Queue[Parameter] = {
    val weightParams = scheme.splits.map(_._2).map(Weight)
    val marginalParams: Seq[Marginal] = for {
      v <- problem.variables.toSeq.sorted
      condVars = scheme.influencesOf(v)
      condition <- IntDomainCPI(condVars.toArray.map(problem.domains).map(Array.range(0,_)))
        .map(assign => condVars.zip(assign).toMap)
    } yield Marginal(v, condition)

    mutable.Queue[Parameter]((marginalParams ++ weightParams).toSeq:_*)
  }

  calibrate(tol,maxIterations)

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
        val weightedFactors: IndexedSeq[FastFactor] =
          for (value <- 0 until problem.domains(vc))
          yield conditionedQs((variable, Map(vc -> value))).map(_ * getProbabilityOfCondition(Map(vc -> value)))
        weightedFactors.reduce[FastFactor]{case (f1,f2) =>
          FastFactor(f1.variables,f1.values.zip(f2.values).map{case (v1,v2) => v1 + v2})
        }
      //a conditioned marginal is requested
      case Seq(vc) if condition.contains(vc) => conditionedQs(variable,Map(vc -> condition(vc)))
      case _ => sys.error("more than one influencing variable found")
    }
  }

  def getProbabilityOfCondition(c: Condition): Double =
    c.foldLeft(1d){case (p,(vc,value)) => cWeights(vc).values(value)}

  def createQDistribution(scope: Array[Int], condition: Condition): FastFactor = {
    val marginals: IndexedSeq[FastFactor] = scope.map(getMarginal(_,condition))
    val conditionEnforcer: FastFactor = FastFactor.deterministicMaxEntropy(
      condition.keys.filter(scope.contains).toArray,
      condition,
      problem.domains,
      NormalD)
    FastFactor.multiply(NormalD)(problem.domains)(marginals :+ conditionEnforcer)
  }

  /** Recalculate the distribution over the conditions induced by the given variable. */
  def computeConditionWeights(variable: Int): FastFactor = {
    require(scheme.splits.exists(_._2 == variable), "argument must be a condition-variable")
    def logZOfCondition(cond: Condition): Double = {
      val entropies = scheme.influencedVariables(variable)
        .collect{case v if v != variable => NormalD.entropy(getMarginal(v,cond).values)}
      val logExpect = scheme.influencedFactors(variable).map(factor =>
        NormalD.expectation(createQDistribution(factor.variables,cond).values,factor.values.map(math.log)))
      logExpect.sum + entropies.sum
    }
    FastFactor.fromFunction(Array(variable),problem.domains, {
      case Array(c) => math.exp(logZOfCondition(Map(variable -> c)))
    }).normalize(NormalD)
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
    //TODO we shouldn't need this line, I think...
    if(condition.contains(variable))
      return FastFactor.deterministicMaxEntropy(Array(variable),condition,problem.domains,NormalD)
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

  def iteration: Int = iterations

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  def variableBelief(vi: Int): FastFactor = getMarginal(vi,Map())

  /** @return Partition function in encoding specified by `ring`. */
  def Z: Double = {
    def expectedVariableEntropy(v: Int): Double = {
      val weightedEntropies =
        for(condition <- scheme.conditionsOf(v))
        yield NormalD.entropy(getMarginal(v,condition).values) * getProbabilityOfCondition(condition)
      weightedEntropies.sum
    }

    val conditionEntropies = cWeights.map(x => NormalD.entropy(x._2.values))
    val logExp = problem.factors.map(logExpectation(_))
    val variableEntropy = problem.variables.map(expectedVariableEntropy)

    math.exp(variableEntropy.sum + logExp.sum + conditionEntropies.sum)
  }

  /** @return The expectation of the factor's log-values, combining all compatible conditions. */
  def logExpectation(f: FastFactor, cond: Condition = Map()): Double = {
    val conditionedExpectations =
      for (condition <- scheme.conditionsOf(f.variables.toSet) if condition.isCompatibleWith(cond))
      yield
        getProbabilityOfCondition(condition) *
          NormalD.expectation(createQDistribution(f.variables, condition).values, f.values.map(math.log))

    conditionedExpectations.sum
  }

  def getProblem: Problem = problem

  def verboseDescription: String = {
    "SinglyLCMF\n" +
    "Scheme:\n" +
     scheme + "\n" +
    "condition weights: \n\t" + cWeights.mkString("\n\t") + "\n" +
    "marginals: \n\t" + conditionedQs.mkString("\n\t")
  }

  /** @return Break-down of contributions to free enery. */
  def freeEnergyReport: String = {
    val conditionEntropies = cWeights.toSeq
      .sortBy(_._1)
      .map { case (cv, dist) => cv -> NormalD.entropy(dist.values)}

    val varEntropies = conditionedQs.toSeq
      .sortBy(_._1._1)
      .map{ case ((v, cond), f) => (v, cond) -> NormalD.entropy(f.values)}

    val logExpectations = for{
      f <- problem.factors
      cond <- scheme.conditionsOf(f.variables.toSet)
    } yield (f,cond) -> logExpectation(f,cond)

    val VEString = varEntropies.map {case ((v, cond),e) => f"$v/$cond:\t$e"}.mkString("\n")
    val CEString = conditionEntropies.map{case (cv,e) => f"$cv:\t$e"}
    val LEString = logExpectations.map{case ((f,cond),le) => f"${f.toBriefString}/$cond:\t$le"}.mkString("\n")

    val varEntropySum: Double = varEntropies.map{case ((_,cond),e) => e * getProbabilityOfCondition(cond)}.sum
    val logZ: Double = conditionEntropies.map(_._2).sum + varEntropySum + logExpectations.map(_._2).sum

    f"""#SimplyLCMF free energy break-down
      |log: $logZ normal: ${math.exp(logZ)}
      |##Condition Entropies
      |total: ${conditionEntropies.map(_._2).sum}
      |$CEString
      |##Variable Entropies
      |total: $varEntropySum
      |$VEString
      |##Log-expectations of factors
      |total: ${logExpectations.map(_._2).sum}
      |$LEString
    """.stripMargin
  }
}