package vultura.factor.inference.gbp

import vultura.factor.ProblemStructure

/** An `OvercountingRegionGraph` is defined by a set of variable-subsets, and an assignment of factors to outer regions.
  * It has its region weights defined automatically as the overcounting numbers. */
@deprecated("thesis cleanup")
case class OvercountingRegionGraph(problemStructure: ProblemStructure,
                                   regions: Set[Set[ProblemStructure#VI]],
                                   factorAssignment: Map[ProblemStructure#FI,Set[ProblemStructure#VI]])
  extends RegionGraph with ChildMapRG with OverCountingNumbers {
  type Region = Set[problemStructure.VI]

  val factorsOfRegion: Map[Region,Set[FI]] =
    (regions.map(r => r -> factorAssignment.filter(_._2 == r).keySet)(collection.breakOut): Map[Region,Set[FI]])
      .withDefaultValue(Set())

  override protected def childrenInitializer(r: Set[problemStructure.VI]): Set[Set[problemStructure.VI]] = {
    val ancs = regions.filter(_.subsetOf(r)) - r
    ancs.filter(r => !ancs.exists(anc => r.subsetOf(anc) && anc != r))
  }

  override def factorsOf(r: Set[VI]): Set[FI] = factorsOfRegion(r)
  override def variablesOf(r: Set[VI]): Set[VI] = r
}
