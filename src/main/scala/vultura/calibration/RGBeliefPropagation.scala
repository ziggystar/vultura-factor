package vultura.calibration

import vultura.factor.inference.{RegionBeliefs, VariationalResult}
import vultura.factor.{Factor, SumProductPowTask, ProblemStructure, Problem}
import vultura.factor.inference.gbp.RegionGraph

/** Implementation of the parent-to-child algorithm to optimize region graph free energies.
  * This is along eq (18) in "Simplifying Generalized Belief Propagation on Redundant Region Graphs" (Wang et Zhou).
  */
case class RGBeliefPropagation(rg: RegionGraph, parameters: Problem) extends CalProblem
with ResultBuilder[RegionBeliefs[RegionGraph#Region] with VariationalResult]{
  if(rg.Diagnosis.isNonRedundant) vultura.factor.inference.logger.warn( "running rgBP on redundant region graph")
  override type N = M

  val ps: ProblemStructure = rg.problemStructure
  type VI = ps.VI
  type FI = ps.FI
  type R = rg.Region

  def msgForEdge(e: (R,R)): M = M(e._1,e._2)

  /** Message for the edge µ→ν. */
  case class M(mu: R, nu: R) extends Node {
    /** Type of dependencies .*/
    override type D = M

    lazy val variables: Array[VI] = rg.variablesOf(nu).toArray.sorted

    /** Size of the array required to store the state of this edge. */
    lazy val arraySize: Int = variables.map(ps.domains).product

    /** The message in the product on the left-hand side of equation (18). This must include `this`. */
    lazy val leftHandSide: IndexedSeq[M] = (for{
          alpha <- rg.boundary(nu) intersect rg.interior(mu)
          gamma <- rg.interior(nu) if rg.edges(alpha -> gamma)
        } yield M(alpha,gamma))(collection.breakOut).distinct

    lazy val denominator: IndexedSeq[M] = leftHandSide.filterNot(_ == this)

    lazy val rightHandSideMessages: IndexedSeq[M] = (for{
          eta <- rg.boundary(mu)
          tau <- rg.interior(mu) -- rg.interior(nu) if rg.edges(eta -> tau)
        } yield M(eta,tau))(collection.breakOut).distinct

    lazy val includedFactors: IndexedSeq[rg.FI] = (for {
          activeRegions <- rg.interior(mu) -- rg.interior(nu)
          fi <- rg.factorsOf(activeRegions)
        } yield fi)(collection.breakOut).distinct

    lazy val dependencies: IndexedSeq[M] = rightHandSideMessages ++ denominator

    lazy val includedFactorParameters: Array[Array[Double]] =
      includedFactors.map(parameters.factors(_).values)(collection.breakOut)

    lazy val task: SumProductPowTask = SumProductPowTask(ps.domains, parameters.ring, variables,
      Seq(
        1d -> includedFactors.map(ps.scopeOfFactor).toArray,
        1d -> rightHandSideMessages.map(_.variables).toArray,
        -1d -> denominator.map(_.variables).toArray)
    )

    /**
      * arguments to SPPT are:
      * 1. the factors
      * 2. the RHS messages
      * 3. the denominator messages
      */
    override def compute(ins: Array[IR], result: IR): Unit = {
      task.sumProduct(includedFactorParameters ++ ins, result)
      parameters.ring.normalizeInplace(result)
    }
  }

  /** Constructs a new initial value for each edge. */
  override def initializer: N => IR = {case m@M(mu, nu) => Array.fill(m.arraySize)(parameters.ring.one)}

    /** The set of nodes defined by this problem. */
  override def nodes: Set[N] = rg.edges.map(msgForEdge)

  override def buildResult(valuation: (M) => IR): RegionBeliefs[RegionGraph#Region] with VariationalResult =
    new RegionBeliefs[RegionGraph#Region] with VariationalResult {
      override def problem: Problem = parameters
      override def regions: Set[RegionGraph#Region] = rg.regions.toSeq.toSet
      override def scopeOfRegion(region: RegionGraph#Region): Set[Int] = rg.variablesOf(region.asInstanceOf[R])
      override def regionBelief(region: RegionGraph#Region): Factor = ???

      override def averageEnergy: Double = ???

      override def entropy: Double = ???

      /** @return marginal distribution of variable in encoding specified by `ring`. */
      override def encodedVarBelief(variable: VI): Factor = ???
    }
}
