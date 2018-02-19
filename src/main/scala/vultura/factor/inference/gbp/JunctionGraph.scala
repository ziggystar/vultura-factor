package vultura.factor.inference.gbp

import vultura.factor.ProblemStructure

/** A two-layer region graph, as described in Yedidia,Freeman,Weiss (Constructing Free...), which is a
  * generalization of the jg described in Aji-McEliece. */
@deprecated("thesis cleanup")
case class JunctionGraph(problemStructure: ProblemStructure,
                         lowerRegions: Set[Set[ProblemStructure#VI]],
                         upperRegions: Set[Set[ProblemStructure#VI]],
                         factorAssignment: Map[ProblemStructure#FI,Set[ProblemStructure#VI]]) extends RegionGraph with ChildMapRG with OverCountingNumbers {
  type Region = Set[VI]
  val regions = lowerRegions ++ upperRegions

  override protected def childrenInitializer(r: Set[VI]): Set[Set[VI]] = if(lowerRegions(r)) Set() else lowerRegions.filter(_.subsetOf(r))

  val factorsOfRegion: Map[Region,Set[FI]] = regions.map(r => r -> factorAssignment.filter(_._2 == r).keySet).toMap.withDefaultValue(Set())

  override def variablesOf(r: Region): Set[VI] = r
  override def factorsOf(r: Region): Set[FI] = factorsOfRegion(r)
}
