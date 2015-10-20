package vultura.factor.inference.gbp

import vultura.factor.{Factor, Ring, ProblemStructure}

trait RegionGraph {
  def problemStructure: ProblemStructure

  type Region

  type VI = ProblemStructure#VI
  type FI = ProblemStructure#FI

  def variablesOf(r: Region): Set[VI]
  def factorsOf(r: Region): Set[FI]
  def weightOf(r: Region): Double


  def regions: Set[Region]
  def parents(r: Region): Set[Region]
  def children(r: Region): Set[Region]
  def ancestors(r: Region): Set[Region]
  def descendants(r: Region): Set[Region]
  def outerRegions: Set[Region] = regions.filter(r => parents(r).isEmpty)
  def innerRegions: Set[Region] = regions -- outerRegions
  /** All descendants and the region itself. */
  def interior(r: Region): Set[Region] = ancestors(r) + r
  /** All regions that are parent to an interior region of `r`, but are not interior themselves. */
  def boundary(r: Region): Set[Region] = interior(r).flatMap(parents) -- interior(r)

  def buildResult(regionBeliefs: Region => Factor, ring: Ring[Double]) = ???
}

case class TwoLayerRG(problemStructure: ProblemStructure,
                      lowerRegions: Set[Set[ProblemStructure#VI]],
                      upperRegions: Set[Set[ProblemStructure#VI]],
                      factorAssignment: Map[ProblemStructure#FI,Set[ProblemStructure#VI]]) extends RegionGraph {
  import vultura.util._

  type Region = Set[VI]
  val regions = lowerRegions ++ upperRegions

  val childMap: Map[Region,Set[Region]] =
    Map(lowerRegions.toSeq.map(_ -> Set[Region]()) ++ upperRegions.toSeq.map(ur => ur -> lowerRegions.filter(_.subsetOf(ur))):_*).withDefaultValue(Set())
  val descendantMap: Map[Set[VI], Set[Set[VI]]] = transitiveClosure(childMap).withDefaultValue(Set())

  val factorsOfRegion: Map[Region,Set[FI]] = regions.map(r => r -> factorAssignment.filter(_._2 == r).keySet).toMap.withDefaultValue(Set())
  val parentMap =  reverseMultiMap(childMap).withDefaultValue(Set())
  val parentMapTrans = transitiveClosure(parentMap).withDefaultValue(Set())

  override def variablesOf(r: Region): Set[VI] = r

  override def children(r: Region): Set[Region] = childMap(r)

  override def factorsOf(r: Region): Set[FI] = factorsOfRegion(r)

  override def descendants(r: Region): Set[Region] = descendantMap(r)

  override def ancestors(r: Region): Set[Region] = parentMapTrans(r)

  override def parents(r: Region): Set[Region] = parentMap(r)

  override def weightOf(r: Region): Double = if(upperRegions(r)) 1d else 1d - parents(r).size

}

object RegionGraph {
  def betheRG(ps: ProblemStructure): TwoLayerRG = TwoLayerRG(ps,
    lowerRegions = ps.variables.map(Set(_))(collection.breakOut),
    upperRegions = ps.scopeOfFactor.map(_.toSet)(collection.breakOut),
    factorAssignment = ps.factorIndices.map(fi => fi -> ps.scopeOfFactor(fi).toSet)(collection.breakOut)
  )
}
