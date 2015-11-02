package vultura.calibration

import vultura.factor._
import vultura.factor.inference._
import vultura.util.SSet

/** Belief propagation on the factor graph representation. Parameters are factor-to-variable messages (F2V), and
  * variable-to-factor messages (V2F). */
case class BeliefPropagation(ps: Problem) extends CalProblem
  with ResultBuilder[RegionBeliefs[Either[Problem#VI, Problem#FI]] with VariationalResult] {
  require(
    new SSet[Int](ps.scopeOfFactor.map(_.toSet).toSet).maximalSets.size == ps.factors.size,
    "no factor scope may be subset of another one, simplify problem first")

  type VI = ps.VI
  type FI = ps.FI

  type N = FactorNode

  val aggregateFactors = new SSet[Int](ps.scopeOfFactor.map(_.toSet).toSet)

  trait FactorNode extends Node {
    def variable: Int
    val arraySize = ps.domains(variable)
  }

  case class V2F(variable: VI, factor: FI) extends FactorNode{
    type D = F2V
    lazy val dependencies: IndexedSeq[F2V] = ps.factorIdxOfVariable(variable).filterNot(_ == factor).map(F2V(_,variable))
    lazy val task: (Array[IR], IR) => Unit =
      SumProductTask(Array(variable),ps.domains,Array.fill(dependencies.size)(Array(variable)), ps.ring).sumProductNormalize(_,_)
    override def compute(ins: Array[IR], result: IR): Unit = task(ins,result)
  }

  case class F2V(factor: FI, variable: VI) extends FactorNode{
    override type D = V2F
    lazy val dependencies: IndexedSeq[V2F] = ps.scopeOfFactor(factor).filterNot(_ == variable).map(V2F(_,factor))
    lazy val task = SumProductTask(
      Array(variable),
      ps.domains,
      dependencies.map(d => Array(d.variable)).toArray :+ ps.scopeOfFactor(factor),
      ps.ring
    )
    private lazy val resultHolder: Array[IR] = {
      val r = new Array[IR](dependencies.size + 1)
      r(dependencies.size) = ps.factors(factor).values
      r
    }
    override def compute(ins: Array[IR], result: IR): Unit = {
      System.arraycopy(ins,0,resultHolder,0,ins.length)
      task.sumProductNormalize(resultHolder,result)
    }
  }

  override def nodes: Set[N] =
    (for (vi <- ps.variables; fi <- ps.factorIdxOfVariable(vi); e <- Seq(F2V(fi, vi), V2F(vi, fi))) yield e)(collection.breakOut)

  /** Constructs a new initial value for each edge. */
  override def initializer: N => IR = e => Array.fill[Double](e.arraySize)(ps.ring.one)

  override def buildResult(valuation: FactorNode => IR)
  : RegionBeliefs[Either[Problem#VI, Problem#FI]] with VariationalResult = {

    def encodedVariableBelief(v: Int): Factor = {
      val incoming = ps.factorIdxOfVariable(v).map{ fi =>
        Factor(Array(v), valuation(F2V(fi,v)))
      }
      Factor.multiply(ps.ring)(ps.domains)(incoming).normalize(ps.ring)
    }

    def encodedFactorBelief(fi: Int): Factor = {
      val incoming = ps.scopeOfFactor(fi).map{ vi =>
        Factor(Array(vi), valuation(V2F(vi,fi)))
      }
      Factor.multiply(ps.ring)(ps.domains)(incoming :+ ps.factors(fi)).normalize(ps.ring)
    }

    new RegionBeliefs[Either[Problem#VI, Problem#FI]] with VariationalResult {
      type R = Either[Problem#VI, Problem#FI]

      override def regions: Set[R] = (ps.variables.map(Left(_)) ++ ps.factorIndices.map(Right(_))).toSet

      override def scopeOfRegion(region: Either[Problem#VI, Problem#FI]): Set[Int] = region match {
        case Left(vi) => Set(vi)
        case Right(fi) => problem.scopeOfFactor(fi).toSet
      }

      def regionEnergy(region: Either[Problem#VI, Problem#FI]): Factor = region match {
        case Left(vi) => Factor.fromFunction(Array(vi), problem.domains, _ => 0d)
        case Right(fi) => if (problem.ring == LogD) problem.factors(fi) else problem.factors(fi).map(math.log)
      }

      override def regionBelief(region: Either[Problem#VI, Problem#FI]): Factor = {
        val encoded = region.fold(encodedVariableBelief,encodedFactorBelief)
        encoded.copy(values = ps.ring.decode(encoded.values))
      }

      /** @return marginal distribution of variable in encoding specified by `ring`. */
      override def encodedVarBelief(variable: Int): Factor = {
        val decoded = regionBelief(Left(variable))
        decoded.copy(values = ps.ring.encode(decoded.values))
      }

      override def problem: Problem = ps

      override def averageEnergy: Double = regions.toSeq.collect{
        case r@Right(fi) =>
          val energy = if (problem.ring == LogD) problem.factors(fi) else problem.factors(fi).map(math.log)
          NormalD.expectation(regionBelief(r).values,energy.values)
      }.sum

      override def entropy: Double = regions.toSeq.map{
        case r@Left(vi) => (1 - ps.degreeOfVariable(vi)) * NormalD.entropy(regionBelief(r).values)
        case r@Right(fi) => NormalD.entropy(regionBelief(r).values)
      }.sum
    }
  }
}

object BeliefPropagation {
  def infer(p: Problem, maxIterations: Long = 100000, tol: Double = 1e-12, damping: Double = 0d)
  : (RegionBeliefs[Either[Problem#VI, Problem#FI]] with VariationalResult, ConvergenceStats) = {
    Calibrator.calibrate(new BeliefPropagation(p))
  }
}