package vultura.factor.inference.calibration

import vultura.factor.inference.{JMIFromRB, MargParI, RegionBeliefs}
import vultura.factor.{Factor, Problem, Var}

/** A mixin to calculate variable beliefs and the log partition function from BP messages. */
trait BPResult extends MargParI with RegionBeliefs[Either[Problem#VI,Problem#FI]] with JMIFromRB[Either[Problem#VI,Problem#FI]] {
  sealed trait Message{
    def vi: Int
    def fi: Int

    def variables: Array[Int] = Array(vi)
    def factorScope: Array[Int] = problem.scopeOfFactor(fi)
    lazy val varIndexInFactorScope: Int = factorScope.indexOf(vi)
  }
  case class V2FMsg(vi: Int, fi: Int) extends Message
  case class F2VMsg(fi: Int, vi: Int) extends Message

  def rawMessageValue(m: Message): Array[Double]

  def messageValue(m: Message): Factor = Factor(m.variables,rawMessageValue(m))

  def problem: Problem

    /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def encodedVarBelief(vi: Int): Factor =
    Factor.multiply(problem.ring)(problem.domains)(problem.factorIdxOfVariable(vi).map(fi => messageValue(F2VMsg(fi,vi)))).normalize(problem.ring)

  def factorBelief(fi: Int): Factor = {
    val f = problem.factors(fi)
    Factor.multiply(problem.ring)(problem.domains)(problem.scopeOfFactor(fi).map(vi => messageValue(V2FMsg(vi,fi))) :+ f).normalize(problem.ring)
  }

  override val regions: Set[Either[Problem#VI, Problem#FI]] = problem.variableSet.map(Left(_)) ++ problem.factorIndices.map(Right(_))

  override def scopeOfRegion(region: Either[Problem#VI, Problem#FI]): Set[Var] = region match {
    case Left(vi) => Set(vi)
    case Right(fi) => problem.scopeOfFactor(fi).toSet
  }

  /** Belief over the variables of a given region.
    * Normal encoding.
    */
  override def regionBelief(region: Either[Problem#VI, Problem#FI]): Factor = (region match {
    case Left(vi) => encodedVarBelief(vi)
    case Right(fi) => factorBelief(fi)
  }).decodeWith(problem.ring)

  /** This is lazy to prevent accessing elements in derived classes early.
   * @return Partition function in encoding specified by `ring`. */
  override lazy val logZ: Double = {
    var logExp: Double = 0d
    var factorEntropy = 0d
    var variableEntropy = 0d

    //variable entropies
    {
      var i = 0
      while (i < problem.numVariables) {
        val vb: Array[Double] = variableBelief(i).values
        val entropy: Double = problem.ring.entropy(vb)
        //check whether we are inconsistent
        if(entropy == 0d && problem.ring.sumA(vb) == problem.ring.zero)
          logExp = Double.NegativeInfinity
        variableEntropy = variableEntropy + entropy * (1 - problem.factorDegreeOfVariable(i))
        i = i + 1
      }
    }

    {
      //factor entropies and factor log-expectations
      var fi = 0
      while (fi < problem.factors.length) {
        val fb: Array[Double] = factorBelief(fi).values
        factorEntropy = factorEntropy + problem.ring.entropy(fb)
        logExp = logExp + problem.ring.logExpectation(fb, problem.factors(fi).values)
        fi += 1
      }
    }

    val result = logExp + factorEntropy + variableEntropy
    result
  }
}
