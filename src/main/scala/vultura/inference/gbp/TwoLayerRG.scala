package vultura.inference.gbp

import vultura.util.SIIndex

/** A bipartite region graph. This includes Bethe graphs, junction graphs, and junction trees. */
trait TwoLayerRG extends RegionGraph {

  protected[TwoLayerRG] def smallRegionsScopes: Set[Set[VI]]
  protected[TwoLayerRG] def largeRegionsScopes: Set[Set[VI]]
  protected[TwoLayerRG] def largeRegionFactors(large: Set[VI]): Set[FI]
  protected[TwoLayerRG] def edgeLabels(from: Set[VI], to: Set[VI]): Set[VI]

  protected[TwoLayerRG] val smallRegionsLookup: SIIndex[Set[VI]] = new SIIndex[Set[VI]](smallRegionsScopes)
  protected[TwoLayerRG] val largeRegionsLookup: SIIndex[Set[VI]] = new SIIndex[Set[VI]](largeRegionsScopes)

  type SI = Int
  type LI = Int
  /** Two layer region type. */
  sealed trait TLR {
    def variables: Set[VI]
    def factors: Set[FI]
    def children: Set[Small]
    def parents: Set[Large]
  }
  final case class Small protected[TwoLayerRG] (si: Int) extends TLR {
    override val variables: Set[VI] = smallRegionsLookup.elements(si)
    override def factors: Set[FI] = Set()
    //parents are all large regions whose variable set subsumes the varset of this region, and the edge has
    //a non-empty label set
    val parentsI: IndexedSeq[LI] = largeRegionsLookup.indices
      .filter{li =>
        val largeVariables: Set[VI] = largeRegionsLookup.backward(li)
        variables.subsetOf(largeVariables) && edgeLabels(largeVariables,variables).nonEmpty
      }
    def parents: Set[Large] = parentsI.map(Large)(collection.breakOut)
    override def children: Set[Small] = Set()

    override def toString: String = s"S(${variables.toSeq.sorted.mkString(",")})"
  }
  final case class Large protected[TwoLayerRG](li: Int) extends TLR {
    override val variables: Set[VI] = largeRegionsLookup.elements(li)
    override val factors: Set[FI] = largeRegionFactors(variables)
    def parents: Set[Large] = Set()
    override def children: Set[Small] =
      variables.flatMap(regionsOfVariable).collect{case s: Small if s.parents.contains(this) => s}

    override def toString: String = s"L(${variables.toSeq.sorted.mkString(",")})"
  }

  val smallRegions: IndexedSeq[Small] = smallRegionsLookup.indices.map(Small)
  val largeRegions: IndexedSeq[Large] = largeRegionsLookup.indices.map(Large)

  type Region = TLR

  override val regions: Set[Region] = (smallRegions ++ largeRegions).toSet
  override def variablesOf(r: Region): Set[VI] = r.variables
  override def factorsOf(r: Region): Set[FI] = r.factors

  /** Direct successors of `r`. */
  override def childrenOf(r: TLR): Set[TLR] = r.children.toSet

  //the following methods *could* be defined through the above methods. */
  override def edgeVariables(parent: TLR, child: TLR): Set[VI] = (parent,child) match {
    case (l: Large, s: Small) => edgeLabels(l.variables,s.variables)
    case _ => Set()
  }
}
