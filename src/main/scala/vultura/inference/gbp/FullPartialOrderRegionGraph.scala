package vultura.inference.gbp

import vultura.factor.ProblemStructure

/** The edges of this region graph are defined by the subset relation between the variable sets associated with regions,
  * with no restriction on the edge variables.
  */
trait FullPartialOrderRegionGraph extends RegionGraph {
  /** Direct successors of `r`. */
  def childrenOf(r: Region): Set[Region] = {
    val scope: Set[VI] = variablesOf(r)
    val descendants = regions.filter(cand => variablesOf(cand).subsetOf(scope)) - r
    //now compute *direct* descendants
    descendants.filter(cand => !descendants.exists(mediator => variablesOf(cand).subsetOf(variablesOf(mediator))))
  }
  def edgeVariables(parent: Region, child: Region): Set[VI] = variablesOf(child)
}

object FullPartialOrderRegionGraph {
  def poRG(ps: ProblemStructure,
           _regions: Iterable[Set[ProblemStructure#VI]],
           factorAssignment: Map[ProblemStructure#FI,Set[ProblemStructure#VI]])
  : FullPartialOrderRegionGraph with OvercountingNumbers =
    new FullPartialOrderRegionGraph with OvercountingNumbers {
    override type Region = Set[ps.VI]
    override val regions: Set[Region] = _regions.toSet
    override val problemStructure: ProblemStructure = ps

    require(factorAssignment.values.forall(regions.contains), "assigning factor to non-existant region")

    override def factorsOf(r: Region): Set[FI] =
      factorAssignment.collect{case (fi,reg) if r == reg => fi}(collection.breakOut)
    override def variablesOf(r: Region): Set[VI] = r
  }
}