package vultura.inference.gbp

import vultura.factor._
import vultura.util.graph.LabeledGraph
import vultura.util.graph.graphviz.DotGraph

import scala.collection.immutable.IndexedSeq

/** Representation of a region graph. */
trait RegionGraph {
  def problemStructure: ProblemStructure

  type Region

  type VI = ProblemStructure#VI
  type FI = ProblemStructure#FI

  def regions: Set[Region]
  def variablesOf(r: Region): Set[VI]
  def factorsOf(r: Region): Set[FI]
  def weightOf(r: Region): Double
  /** Direct successors of `r`. */
  def childrenOf(r: Region): Set[Region]
  //the following methods *could* be defined through the above methods. */
  def edgeVariables(parent: Region, child: Region): Set[VI]

  def parentsOf(r: Region): Set[Region] = directedGraph.parents(r)
  def edges: Set[(Region,Region)] = directedGraph.edges
  /** Transitive closure of `parents` for `r`. */
  def ancestors(r: Region): Set[Region] = directedGraph.ancestors(r)
  /** Transitive closure of `children` for `r`. */
  def descendants(r: Region): Set[Region] = directedGraph.descendants(r)
  /** All descendants and the region itself. */
  def interior(r: Region): Set[Region] = ancestors(r) + r
  /** All regions that are parent to an interior region of `r`, but are not interior themselves. */
  def boundary(r: Region): Set[Region] = {
    val int = interior(r)
    for{
      i <- int
      p <- parentsOf(i) if !int.contains(p)
    } yield p
  }

  lazy val regionsOfVariable: IndexedSeq[Set[Region]] = problemStructure.variables.map{vi =>
    regions.filter(r => variablesOf(r).contains(vi))
  }

  def regionsWithVariables(vs: Iterable[VI]): Set[Region] = {
    val s = vs.toSet
    s.map(regionsOfVariable).reduce(_ intersect _)
  }
  def regionsWithFactors(fs: Iterable[FI]): Set[Region] = {
    val f = fs.toSet
    regions.filter(r => f.subsetOf(factorsOf(r)))
  }

  lazy val directedGraph: LabeledGraph[Region] = LabeledGraph.fromChildList(regions,r => childrenOf(r))
  def toDot: DotGraph[Region, (Region,Region)] = DotGraph(regions,edges).labelNodes{case r => weightOf(r).toString}
}
