package vultura.calibration

import com.typesafe.scalalogging.StrictLogging
import vultura.factor.inference.{VarBeliefFromRegionBelief, RegionBeliefs, VariationalResult}
import vultura.factor._
import vultura.factor.inference.gbp.RegionGraph

/** Implementation of the parent-to-child algorithm to optimize region graph free energies.
  * This is along eq (18) in "Simplifying Generalized Belief Propagation on Redundant Region Graphs" (Wang et Zhou).
  */
case class RGBeliefPropagation(rg: RegionGraph, parameters: Problem) extends CalProblem
with StrictLogging with ResultBuilder[RegionBeliefs[RegionGraph#Region] with VariationalResult]{
  if(rg.Diagnosis.isNonRedundant && logger.underlying.isWarnEnabled)
    logger.warn( "running rgBP on redundant region graph")
  override type N = M

  val ps: ProblemStructure = rg.problemStructure
  type VI = ps.VI
  type FI = ps.FI
  type R = rg.Region

  def msgForEdge(e: (R,R)): M = M(e._1,e._2)

  /** Message for the edge µ→ν. */
  case class M(mu: R, nu: R) extends Node {
    require(rg.edges.contains(mu -> nu), "instantiating non-existent region-graph edge")
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
    new RegionBeliefs[RegionGraph#Region] with VariationalResult with VarBeliefFromRegionBelief[RegionGraph#Region] {
      override def problem: Problem = parameters
      override def regions: Set[RegionGraph#Region] = rg.regions.toSeq.toSet
      override def scopeOfRegion(region: RegionGraph#Region): Set[Int] = rg.variablesOf(region.asInstanceOf[R])

      override def averageEnergy: Double = rg.regions.filter(rg.factorsOf(_).nonEmpty).foldLeft(0d){case (ae,r) =>
          val factorIdx = rg.factorsOf(r)
          val rbel = regionBelief(r)
          //sum the log-factors (which is aquivalent to multiplying their normal values in log encoding
          val joint = Factor.multiplyRetain(LogD)(ps.domains)(factorIdx.toIndexedSeq.map(problem.logFactor),rbel.variables)
          val regionEnergy = NormalD.expectation(rbel.values,joint.values)
          ae + rg.weightOf(r) * regionEnergy
      }
      override def entropy: Double = rg.regions.foldLeft(0d){case (h,r) =>
        val hr = NormalD.entropy(regionBelief(r).values)
          h + rg.weightOf(r) * hr
      }

      /** This is equation (14). */
      override def regionBelief(region: RegionGraph#Region): Factor = {
        import rg._

        val r = region.asInstanceOf[rg.Region]
        val potentials: IndexedSeq[Factor] = factorsOf(r).map(parameters.factors)(collection.breakOut)
        val messages: IndexedSeq[M] = (for {
                  mu <- boundary(r)
                  nu <- interior(r) if edges.contains(mu -> nu)
                } yield M(mu,nu))(collection.breakOut)
        val messageFactors: IndexedSeq[Factor] = messages.map(m => Factor(m.variables,valuation(m)))
        val encoded =
          Factor.multiply(parameters.ring)(ps.domains)(potentials ++ messageFactors).normalize(problem.ring)
        encoded.copy(values = problem.ring.decode(encoded.values))
      }
    }
}
