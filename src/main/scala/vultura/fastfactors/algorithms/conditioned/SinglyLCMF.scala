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

  sealed trait Parameter{ def effect: Set[Parameter]}
  case class Marginal(variable: Int, condition: Condition) extends Parameter{
    def effect: Set[Parameter] = for{
      n <- problem.neighboursOf(variable) if !scheme.conditionVariables(n)
      cond <- scheme.conditionsOf(n) if cond.isCompatibleWith(condition)
      eff <- Seq(Marginal(n,cond)) ++ cond.headOption.map{case (cv,_) => Seq(Weight(cv))}.getOrElse(Seq())
    } yield eff

    override def toString: String = f"b($variable|${printCondition(condition)})"
  }
  case class Weight(variable: Int) extends Parameter {
    def effect: Set[Parameter] = {
      val influencedVariables = scheme.influencedVariables(variable)
      influencedVariables
        .flatMap(problem.neighboursOf)
        .filterNot(influencedVariables)
        .map(Marginal(_,Map()))
    }

    override def toString: String = f"c($variable)"
  }
  val allWeightParams: Seq[Weight] = scheme.conditionVariables.map(Weight)(collection.breakOut)
  val allMarginalParams: Seq[Marginal] = for {
    v <- problem.variables.toSeq.sorted if !scheme.conditionVariables(v)
    condVars = scheme.influencesOf(v)
    condition <- IntDomainCPI(condVars.toArray.map(problem.domains).map(Array.range(0,_)))
      .map(assign => condVars.zip(assign).toMap)
  } yield Marginal(v, condition)

  /* members for representation of the approximate distribution */
  /** The normalized distribution over the conditioning variables. */
  private val cWeights: mutable.Map[Int,FastFactor] = new mutable.HashMap[Int,FastFactor]()
    .withDefault(variable => FastFactor.maxEntropy(Array(variable),problem.domains,problem.ring))
  /**
   * The marginals of the variables under conditions;
   * together with `cWeights`, these define the approximate distribution.
   */
  private val conditionedQs: mutable.Map[(Int,Condition),FastFactor] =
    new mutable.HashMap[(Int,Condition),FastFactor]().withDefault((createInitialMarginal _).tupled)

  /* book-keeping members */
  private var iterations: Int = 0

  /** The queue holding the parameters to recalculate before possible convergence. */
  private val uncalibrated: mutable.Queue[Parameter] = mutable.Queue[Parameter](allMarginalParams ++ allWeightParams:_*)

  /* optimization members */
  /** The logarithmic factors of the problem adjacent to each variable. This is just a cache for
    * faster computation. */
  val logFactors: Map[Int,IndexedSeq[FastFactor]] = {
    def logFactorsOf(v: Int): IndexedSeq[FastFactor] =
      problem.factorsOfVariable(v).map(ff => ff.copy(values = LogD.encode(ff.values)))
    problem.variables.map(v => v -> logFactorsOf(v))(collection.breakOut)
  }
  /* end optimization members */

  calibrate(tol,maxIterations)

  /** Build the initial marginal belief for a given variable and condition .*/
  def createInitialMarginal(variable: Int, condition: Condition): FastFactor = {
    require(condition.keySet == scheme.influencesOf(variable))
    FastFactor.deterministicMaxEntropy(Array(variable), condition, problem.domains, problem.ring)
  }

  /** Compute the estimated probability of a given condition. */
  def probabilityOfCondition(c: Condition): Double = c.foldLeft(1d){case (p,(vc,value)) => cWeights(vc).values(value)}

  /** Create the estimated distribution over some given variables under a certain condition. */
  def estimatedDistribution(scope: Array[Int], condition: Condition = Map()): FastFactor = {
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
            yield conditionedQs((variable, Map(vc -> value))).map(_ * probabilityOfCondition(Map(vc -> value)))
          weightedFactors.reduce[FastFactor]{case (f1,f2) =>
            FastFactor(f1.variables,f1.values.zip(f2.values).map{case (v1,v2) => v1 + v2})
          }
        //a conditioned marginal is requested
        case Seq(vc) if condition.contains(vc) => conditionedQs(variable,Map(vc -> condition(vc)))
        case _ => sys.error("more than one influencing variable found")
      }
    }

    val marginals: IndexedSeq[FastFactor] = scope.map(getMarginal(_,condition))
    val conditionEnforcer: FastFactor = FastFactor.deterministicMaxEntropy(
      condition.keys.filter(scope.contains).toArray,
      condition,
      problem.domains,
      NormalD)
    FastFactor.multiply(NormalD)(problem.domains)(marginals :+ conditionEnforcer)
  }

  /** Recalculate the distribution over the conditions induced by the given variable. */
  def computeNewConditionWeights(variable: Int): FastFactor = {
    require(scheme.splits.exists(_._2 == variable), "argument must be a condition-variable")

    def logZOfCondition(cond: Condition): Double = {
      val entropies = scheme.influencedVariables(variable)
        .filterNot(variable ==)
        .map(v => NormalD.entropy(estimatedDistribution(Array(v),cond).values))
      val logExpect = scheme.influencedFactors(variable).map(logExpectation(_,cond))
      logExpect.sum + entropies.sum
    }
    FastFactor.fromFunction(Array(variable),problem.domains, {
      case Array(c) => math.exp(logZOfCondition(Map(variable -> c)))
    }).normalize(NormalD)
  }

  /** Calculate the marginal distribution for a given variable and condition. No side-effects. */
  def computeNewMarginal(variable: Int, condition: Condition): FastFactor = {
    require(!condition.contains(variable))

    val factorContribs: IndexedSeq[FastFactor] = for {
      logFactor <- logFactors(variable)
      qDist = estimatedDistribution(logFactor.variables.filterNot(_ == variable),condition)
      expectedLog = FastFactor.multiplyRetain(NormalD)(problem.domains)(Array(logFactor, qDist), Array(variable))
    } yield expectedLog
    FastFactor.elementWiseSum(factorContribs).map(math.exp).normalize(NormalD)
  }

  /** Update the weights for the conditions induced by a given variable.
    * @return `true`, if the the value was changed. */
  private def updateConditionWeights(variable: Int): Boolean = {
    val newWeights = computeNewConditionWeights(variable)
    val diff = FastFactor.maxDiff(cWeights(variable),newWeights,NormalD)
    if(diff > tol){
      cWeights.put(variable,newWeights)
      return true
    }
    false
  }

  /** Update the stored marginal distribution for a given variable and condition.
    * @return `true`, if the the value was changed. */
  private def updateQ(variable: Int, condition: Condition): Boolean = {
    val newDist = computeNewMarginal(variable,condition)
    val diff = FastFactor.maxDiff(conditionedQs(variable -> condition),newDist,NormalD)
    if(diff > tol){
      conditionedQs.put(variable -> condition,newDist)
      return true
    }
    false
  }

  /** @return The expectation of the factor's log-values conditional on `cond` */
  def logExpectation(f: FastFactor, cond: Condition = Map()): Double = {
    val conditionedExpectations: Set[(Double, Double)] =
      for (condition <- scheme.conditionsOf(f.variables.toSet) if condition.isCompatibleWith(cond))
      yield
        probabilityOfCondition(condition) ->
          NormalD.expectation(estimatedDistribution(f.variables, condition).values, f.values.map(math.log))
    //normalize the weighted sum by the probability of the condition
    conditionedExpectations.map(x => x._1 * x._2).sum / conditionedExpectations.map(_._1).sum
  }

  /** The expected entropy of the estimated distribution over one variable, given a condition. */
  def expectedVariableEntropy(v: Int, cond: Condition = Map()): Double = {
    val weightedEntropies =
      for(condition <- scheme.conditionsOf(v) if condition.isCompatibleWith(cond))
      yield probabilityOfCondition(condition) -> NormalD.entropy(estimatedDistribution(Array(v),condition).values)
    weightedEntropies.map(x => x._1 * x._2).sum * weightedEntropies.map(_._1).sum
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
  def variableBelief(vi: Int): FastFactor = estimatedDistribution(Array(vi))

  /** @return Partition function in encoding specified by `ring`. */
  def Z: Double = {
    val conditionEntropies = cWeights.map(x => NormalD.entropy(x._2.values))
    val logExp = problem.factors.map(logExpectation(_))
    val variableEntropy = problem.variables.map(expectedVariableEntropy(_))

    math.exp(variableEntropy.sum + logExp.sum + conditionEntropies.sum)
  }

  def getProblem: Problem = problem

  def verboseDescription: String = {
    "SinglyLCMF\n" +
    "Scheme:\n" +
     scheme + "\n" +
    "condition weights: \n\t" + cWeights.mkString("\n\t") + "\n" +
    "marginals: \n\t" + conditionedQs.mkString("\n\t")
  }

  /** @return Break-down of contributions to free energy. */
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

    val varEntropySum: Double = varEntropies.map{case ((_,cond),e) => e * probabilityOfCondition(cond)}.sum
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
  
  def parameterDependencyGraph: String = {
    //produce a string that encodes the condition and is allowed as part of a node name in graphiz
    def cNN(c: Condition): String = c.map{case (k,v) => f"${k}x$v"}.mkString("S")
    def pString(p: Parameter): String = p match {
      case Marginal(v,c) => f"m${v}_${cNN(c)}"
      case Weight(v) => f"w$v"
    }
    val weights = for{
      w <- allWeightParams ++ allMarginalParams
      succ <- w.effect
    } yield f"\t${pString(w)} -> ${pString(succ)}"

    f"""digraph LCMF-DepGraph{
      |${weights.mkString(";\n")}
      |}
    """.stripMargin
  }
}