package vultura.calibration

import vultura.factor._
import vultura.factor.inference._
import vultura.util.SSet
import vultura.util.graph2.graphviz.Directed

/** Belief propagation on the factor graph representation. */
case class BetheProblem(ps: Problem) extends CalProblem {
  require(
    new SSet[Int](ps.scopeOfFactor.map(_.toSet).toSet).maximalSets.size == ps.factors.size,
    "no factor scope may be subset of another one, simplify problem first")

  type VI = ps.VI

  type FI = ps.FI

  val aggregateFactors = new SSet[Int](ps.scopeOfFactor.map(_.toSet).toSet)

  trait FactorEdge extends Edge {
    def variable: Int
    val arraySize = ps.domains(variable)
  }

  case class V2F(variable: VI, factor: FI) extends FactorEdge{
    type D = F2V
    lazy val dependencies: IndexedSeq[F2V] = ps.factorIdxOfVariable(variable).filterNot(_ == factor).map(F2V(_,variable))
    lazy val task: (Array[LRep], LRep) => Unit =
      SumProductTask(Array(variable),ps.domains,Array.fill(dependencies.size)(Array(variable)), ps.ring).sumProductNormalize(_,_)
    override def compute(ins: Array[LRep], result: LRep): Unit = task(ins,result)
  }

  case class F2V(factor: FI, variable: VI) extends FactorEdge{
    override type D = V2F
    lazy val dependencies: IndexedSeq[V2F] = ps.scopeOfFactor(factor).filterNot(_ == variable).map(V2F(_,factor))
    lazy val task = SumProductTask(
      Array(variable),
      ps.domains,
      dependencies.map(d => Array(d.variable)).toArray :+ ps.scopeOfFactor(factor),
      ps.ring
    )
    private lazy val resultHolder: Array[LRep] = {
      val r = new Array[LRep](dependencies.size + 1)
      r(dependencies.size) = ps.factors(factor).values
      r
    }
    override def compute(ins: Array[LRep], result: LRep): Unit = {
      System.arraycopy(ins,0,resultHolder,0,ins.length)
      task.sumProductNormalize(resultHolder,result)
    }
  }

  override def edges: Set[Edge] =
    (for (vi <- ps.variables; fi <- ps.factorIdxOfVariable(vi); e <- Seq(F2V(fi, vi), V2F(vi, fi))) yield e)(collection.breakOut)

  /** Constructs a new initial value for each edge. */
  override def initializer: Edge => LRep = e => Array.fill[Double](e.arraySize)(ps.ring.one)

  def buildResult(valuation: Edge => LRep): MargParI = ???
}

object BetheProblem {
  type ResultType = RegionBeliefs[Either[Problem#VI, Problem#FI]] with VariationalResult
  /**
   * @param p Inference problem.
   * @param tol Maximum difference in element-wise message values that is considered converged.
   * @param maxIterations Maximum number of outer loops (all edges may be updated in one iteration).
   * @param damping 0 is no damping, 1 is no frozen (illegal).
   */
  def infer(p: Problem, tol: Double = 1e-12, maxIterations: Long = 100000, damping: Double = 0): (ResultType,ConvergenceStats) = {

    val calProb = BetheProblem(p)
    val calibrator = new Calibrator[calProb.type](calProb)

    val stats = calibrator.calibrate(maxIterations,tol,damping)

    val labeled = calibrator.computationGraph.labelNodes { case x =>
      s"$x\n${calibrator.edgeState(x).map(_.formatted("%.3f")).mkString(",")}"
    }

    def encodedVariableBelief(v: Int): Factor = {
      val incoming = p.factorIdxOfVariable(v).map{ fi =>
        Factor(Array(v), calibrator.edgeState(calibrator.cp.F2V(fi,v)))
      }
      Factor.multiply(p.ring)(p.domains)(incoming).normalize(p.ring)
    }

    def encodedFactorBelief(fi: Int): Factor = {
      val incoming = p.scopeOfFactor(fi).map{ vi =>
        Factor(Array(vi), calibrator.edgeState(calibrator.cp.V2F(vi,fi)))
      }
      Factor.multiply(p.ring)(p.domains)(incoming :+ p.factors(fi)).normalize(p.ring)
    }


    val result = new RegionBeliefs[Either[Problem#VI, Problem#FI]] with VariationalResult {
      type R = Either[Problem#VI, Problem#FI]

      override def regions: Set[R] = (p.variables.map(Left(_)) ++ p.factorIndices.map(Right(_))).toSet

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
        encoded.copy(values = p.ring.decode(encoded.values))
      }

      /** @return marginal distribution of variable in encoding specified by `ring`. */
      override def encodedVarBelief(variable: Int): Factor = {
        val decoded = regionBelief(Left(variable))
        decoded.copy(values = p.ring.encode(decoded.values))
      }

      override def problem: Problem = p

      override def averageEnergy: Double = regions.toSeq.collect{
        case r@Right(fi) =>
          val energy = if (problem.ring == LogD) problem.factors(fi) else problem.factors(fi).map(math.log)
          NormalD.expectation(regionBelief(r).values,energy.values)
      }.sum

      override def entropy: Double = regions.toSeq.map{
        case r@Left(vi) => (1-p.degreeOfVariable(vi)) * NormalD.entropy(regionBelief(r).values)
        case r@Right(fi) => NormalD.entropy(regionBelief(r).values)
      }.sum
    }

    (result,stats)
  }
}
