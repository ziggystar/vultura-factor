package vultura.calibration

import vultura.factor.inference.{RegionBeliefs, VariationalResult}
import vultura.factor.{ProblemStructure, Problem}
import vultura.factor.inference.gbp.RegionGraph

/** Implementation of the parent-to-child algorithm to optimize region graph free energies.
  * What about
  *  - redundancy?
  */
case class RGBeliefPropagation(rg: RegionGraph, parameters: Problem) extends CalProblem
with ResultBuilder[RegionBeliefs[RegionGraph#Region] with VariationalResult]{
  if(rg.isNonRedundant.nonEmpty) vultura.factor.inference.logger.warn( "running rgBP on redundant region graph")
  override type N = P2C

  val ps: ProblemStructure = rg.problemStructure
  type VI = ps.VI
  type FI = ps.FI
  type R = rg.Region

  case class P2C(parent: R, child: R) extends Node {
    /** Type of dependencies .*/
    override type D = P2C

    lazy val variables: Array[VI] = rg.variablesOf(child).toArray.sorted

    /** Size of the array required to store the state of this edge. */
    final override def arraySize: Int = variables.map(ps.domains).product

    override def dependencies: IndexedSeq[P2C] = ???

    /**
     * - first parameter: `zip`s with `dependencies`.
     * - second parameter: Result of computation shall be stored here. Content of result is garbage.
     */
    override def compute(ins: Array[IR], result: IR): Unit = ???
  }

  /** Constructs a new initial value for each edge. */
  override def initializer: N => IR = ???

  /** The set of nodes defined by this problem. */
  override def nodes: Set[N] = ???


  override def buildResult(valuation: (P2C) => IR): RegionBeliefs[RegionGraph#Region] with VariationalResult = ???
}
