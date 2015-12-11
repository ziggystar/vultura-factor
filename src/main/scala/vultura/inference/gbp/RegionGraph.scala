package vultura.inference.gbp

import vultura.factor._
import vultura.util.{SIIndex, SSet}
import vultura.util.graph.graphviz.DotGraph
import vultura.util.graph.{LabeledGraph, Tree}

import scala.collection.immutable.IndexedSeq

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

  def regionsWithVariables(vs: Iterable[VI]): Set[Region] = {
    val s = vs.toSet
    regions.filter(r => s.subsetOf(variablesOf(r)))
  }
  def regionsWithFactors(fs: Iterable[FI]): Set[Region] = {
    val f = fs.toSet
    regions.filter(r => f.subsetOf(factorsOf(r)))
  }

  lazy val directedGraph: LabeledGraph[Region] = LabeledGraph.fromChildList(regions,r => childrenOf(r))
  def toDot: DotGraph[Region, (Region,Region)] = DotGraph.apply(regions,edges).labelNodes{case r => weightOf(r).toString}
}

trait OvercountingNumbers { self: RegionGraph =>
  protected lazy val overCountingNumbers: Map[Region,Int] = {
    val topological = directedGraph.tarjanSCC.map{
      case ns if ns.size == 1 => ns.head
      case _ => sys.error("region graph graph contains directed cycles")
    }
    topological.foldLeft(Map[Region,Int]()){case (m,r) => m + (r -> (1 - ancestors(r).foldLeft(0)(_ + m(_))))}
  }
  final override def weightOf(r: Region): Double = overCountingNumbers(r).toDouble
}

/** The edges of this region graph are defined by the subset relation between the variable sets associated with regions,
  * with no restriction on the edge variables.
  */
trait FullPartialOrderRegionGraph extends RegionGraph {
  /** Direct successors of `r`. */
  def childrenOf(r: Region): Set[Region] = {
    val set: Set[VI] = variablesOf(r)
    regions.filter(childCand => variablesOf(childCand).subsetOf(set))
  }
  def edgeVariables(parent: Region, child: Region): Set[VI] = variablesOf(child)
}

trait TwoLayerRG extends RegionGraph {

  protected[TwoLayerRG] def smallRegionsData: Set[Set[VI]]
  protected[TwoLayerRG] def largeRegionsData: Set[(Set[VI],Set[FI])]

  protected[TwoLayerRG] val smallRegionsLookup: SIIndex[Set[VI]] = new SIIndex[Set[VI]](smallRegionsData)
  protected[TwoLayerRG] val largeRegionsLookup: SIIndex[(Set[VI],Set[FI])] = new SIIndex[(Set[VI], Set[FI])](largeRegionsData)

  sealed trait TLR {
    def variables: Set[VI]
    def factors: Set[FI]
    def children: Seq[Small]
    def parents: Seq[Large]
  }
  case class Small protected[TwoLayerRG] (si: Int) extends TLR {
    override def variables: Set[VI] = smallRegionsLookup.elements(si)
    override def factors: Set[FI] = Set()
    val parentsI: IndexedSeq[VI] =
      largeRegionsLookup.indices.filter(li => variables.subsetOf(largeRegionsLookup.backward(li)._1))
    def parents: Seq[Large] = parentsI.map(Large)
    override def children: Seq[Small] = Seq()
  }
  case class Large protected[TwoLayerRG](li: Int) extends TLR {
    override def variables: Set[VI] = largeRegionsLookup.elements(li)._1
    override def factors: Set[FI] = largeRegionsLookup.elements(li)._2
    def parents: Seq[Large] = Seq()
    override def children: Seq[Small] = smallRegions.filter(_.parents.contains(this))
  }

  val smallRegions: IndexedSeq[Small] = smallRegionsLookup.indices.map(Small)
  val largeRegions: IndexedSeq[Large] = largeRegionsLookup.indices.map(Large)

  type Region = TLR

  override def regions: Set[Region] = (smallRegions ++ largeRegions).toSet
  override def variablesOf(r: Region): Set[VI] = r.variables
  override def factorsOf(r: Region): Set[FI] = r.factors

  /** Direct successors of `r`. */
  override def childrenOf(r: TLR): Set[TLR] = r.children.toSet

  //the following methods *could* be defined through the above methods. */
  override def edgeVariables(parent: TLR, child: TLR): Set[VI] = child.variables
}

/** Two-layered region graph with overcounting numbers.
  *
  * @param largeRegionsData first tuple entry is the variable scope of the region, second are the indices of the included factors
  * @param smallRegionsData a small region is just a variable scope
  */
case class TwoLayerOC(problemStructure: ProblemStructure, largeRegionsData: Set[(Set[Int],Set[Int])], smallRegionsData: Set[Set[Int]])
  extends TwoLayerRG with OvercountingNumbers

object TwoLayerOC {
  /** Constructs a Bethe region graph.
    * The construction aggregates factors with containing/subsumed scopes to avoid redundancy. */
  def betheRegionGraph(ps: ProblemStructure): TwoLayerOC = {
    val scopeLookup = new SSet[Int](ps.scopeOfFactor.map(_.toSet)(collection.breakOut))
    val largeRegions: Set[(Set[Int], Set[Int])] = scopeLookup.maximalSets.map { maxSet =>
      val factors = maxSet.map(ps.factorIdxOfVariable(_).toSet).reduce(_ intersect _)
      maxSet -> factors
    }

    TwoLayerOC(ps, largeRegions, ps.variableSet.map(Set(_)))
  }

  def junctionTreeMinDegree(problemStructure: ProblemStructure): TwoLayerOC = {
    import vultura.util.TreeWidth._

    type VI = problemStructure.VI
    type FI = problemStructure.FI

    val variableOrder: Seq[VI] = treeDecomposition(problemStructure.scopeOfFactor, problemStructure.domains)
      .getOrElse(sys.error("could not find a small tree decomposition"))

    val rawJunctionTrees: Seq[Tree[(Set[VI], Seq[FI])]] = junctionTreesFromOrder(
      problemStructure.factorIndices.map(fi => problemStructure.scopeOfFactor(fi).toSet -> fi),
      variableOrder)

    //1. create format for jt-creation
    //2. merge all factors of each clique into one
    val compactedTrees: Seq[Tree[(Set[VI], Seq[FI])]] = compactJTrees(rawJunctionTrees)

    val largeRegions: Set[(Set[VI], Set[FI])] = compactedTrees.flatMap(_.flatten).map{
      case (vs,fs) => (vs,fs.toSet)
    }(collection.breakOut)

    def slidingTree[A](t: Tree[A]): Stream[(A,A)] = t match {
      case Tree.Node(a, leafs)    => leafs.map(l => (a,l.rootLabel)) ++ leafs.flatMap(slidingTree)
    }
    val smallRegions: Set[Set[VI]] = compactedTrees.flatMap(tree =>
      slidingTree(tree).map{case ((pa,_),(ca,_)) => pa intersect ca}
    )(collection.breakOut)

    TwoLayerOC(problemStructure, largeRegions, smallRegions)
  }
}