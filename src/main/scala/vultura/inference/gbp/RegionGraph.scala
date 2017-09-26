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

trait OvercountingNumbers { self: RegionGraph =>
  protected lazy val overCountingNumbers: Map[Region,Int] = {
    val topological = directedGraph.tarjanSCC.map{
      case ns if ns.size == 1 => ns.head
      case _ => sys.error("region graph graph contains directed cycles")
    }.reverse
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
    val scope: Set[VI] = variablesOf(r)
    val descendants = regions.filter(cand => variablesOf(cand).subsetOf(scope)) - r
    //now compute *direct* descandants
    descendants.filter(cand => !descendants.exists(mediator => variablesOf(cand).subsetOf(variablesOf(mediator))))
  }
  def edgeVariables(parent: Region, child: Region): Set[VI] = variablesOf(child)
}

object FullPartialOrderRegionGraph {
  def poRG(ps: ProblemStructure,
           _regions: Iterable[Set[ProblemStructure#VI]],
           factorAssignment: Map[ProblemStructure#FI,Set[ProblemStructure#VI]]): FullPartialOrderRegionGraph with OvercountingNumbers =
    new FullPartialOrderRegionGraph with OvercountingNumbers {
    override type Region = Set[ps.VI]
    override val regions: Set[Region] = _regions.toSet
    override val problemStructure: ProblemStructure = ps

    require(factorAssignment.values.forall(regions.contains), "assigning factor to non-existant region")

    override def factorsOf(r: Region): Set[FI] = factorAssignment.collect{case (fi,reg) if r == reg => fi}(collection.breakOut)
    override def variablesOf(r: Region): Set[VI] = r
  }
}

case class RgDiagnosis(rg: RegionGraph){
  lazy val validityChecks: Seq[(String,() => Boolean)] = Seq(
    "variables are connected" -> (() => variablesAreConnected),
    "factors are connected" -> (() => factorsAreConnected),
    "weights of regions containing a variable don't sum to 1" -> (() => variablesAreCountedOnce),
    "weights of regions containing a factor don't sum to 1" -> (() => factorsAreCountedOnce)
  )

  lazy val validityIssues: Seq[String] = validityChecks.collect{case (msg,v) if !v() => msg}

  lazy val isValidRegionGraph: Boolean = validityIssues.isEmpty

  /** Redundancy means that each variable-induced sub-graph is a tree. */
  def nonRedundantVariables: IndexedSeq[rg.VI] = rg.problemStructure.variables
    .filterNot(v => rg.directedGraph.filterNodes(rg.regionsOfVariable(v)).isTree)
  def isNonRedundant: Boolean = nonRedundantVariables.nonEmpty

  def variablesAreCountedOnce: Boolean = rg.problemStructure.variables
    .exists(vi => math.abs(rg.regionsOfVariable(vi).map(rg.weightOf).sum - 1d) > 1e-7)

  def factorsAreCountedOnce: Boolean = rg.problemStructure.factorIndices
    .forall(fi => math.abs(rg.regionsWithFactors(Seq(fi)).toSeq.map(rg.weightOf).sum - 1d) < 1e-7)

  def factorsAreInOuterRegions: Boolean = rg.regions.exists(r => rg.parentsOf(r).nonEmpty && rg.factorsOf(r).nonEmpty)

  def variablesAreConnected: Boolean = rg.problemStructure.variables
    .forall(v => rg.directedGraph.filterNodes(rg.regionsOfVariable(v)).connectedComponents.size == 1)
  def factorsAreConnected: Boolean = rg.problemStructure.factorIndices
    .forall{fi => rg.directedGraph.filterNodes(rg.regionsWithVariables(rg.problemStructure.scopeOfFactor(fi).toSet)).connectedComponents.size == 1 }
}

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

/** Two-layered region graph with overcounting numbers.
  * @param factorsOfLargeRegions Maps a large region (represented by the set of included variable indices)
  *   to a set of factor indices. */
class TwoLayerOC(val problemStructure:      ProblemStructure,
                 val smallRegionsScopes:    Set[Set[Int]],
                 val largeRegionsScopes:    Set[Set[Int]],
                 val factorsOfLargeRegions: Set[Int] => Set[Int],
                 val separatorSets:         (Set[Int],Set[Int]) => Set[Int])
  extends TwoLayerRG with OvercountingNumbers {
  override def largeRegionFactors(large: Set[VI]): Set[FI] = factorsOfLargeRegions(large)
  override def edgeLabels(from: Set[VI], to: Set[VI]): Set[VI] = separatorSets(from,to)

  val regionsWithVariable: IndexedSeq[Set[TLR]] = {
    require(problemStructure.variables.min == 0 && problemStructure.variables.step == 1)
    problemStructure.variables.map{vi => regions.filter(r => variablesOf(r).contains(vi))}
  }
  override def regionsWithVariables(vs: Iterable[VI]): Set[TLR] = {
    val set = vs.toSet
    set.flatMap(regionsWithVariable).filter(r => set subsetOf variablesOf(r))
  }
}

object TwoLayerOC {
  /** Constructs a Bethe region graph.
    * The construction aggregates factors with containing/subsumed scopes to avoid redundancy. */
  def betheRegionGraph(ps: ProblemStructure, aggregateFactors: Boolean = true): TwoLayerOC = {
    val scopeLookup = new SSet[Int](ps.scopeOfFactor.map(_.toSet)(collection.breakOut))
    val largeRegions: Map[Set[ps.VI],Set[ps.FI]] = if(aggregateFactors)
      ps.factorIndices.toSet.groupBy(fi => scopeLookup.maximalSuperSetsOf(ps.scopeOfFactor(fi).toSet).head)
    else
      ps.factorIndices.toSet.groupBy(fi => ps.scopeOfFactor(fi).toSet)

    new TwoLayerOC(
      problemStructure = ps,
      smallRegionsScopes = ps.variableSet.map(Set(_)),
      largeRegionsScopes = largeRegions.keySet,
      factorsOfLargeRegions = largeRegions.apply,
      separatorSets = {case (large,small) if small.subsetOf(large) => small})
  }

  def junctionTreeMinDegree(problemStructure: ProblemStructure): TwoLayerOC = {
    import vultura.util.TreeWidth._
    val variableOrder: Seq[Int] = treeDecomposition(problemStructure.scopeOfFactor, problemStructure.domains)
      .getOrElse(sys.error("could not find a small tree decomposition"))

    junctionTree(problemStructure,variableOrder)
  }

  def junctionTree(problemStructure: ProblemStructure, variableOrder: Seq[Int]): TwoLayerOC = {
    import vultura.util.TreeWidth._

    type VI = problemStructure.VI
    type FI = problemStructure.FI

    val rawJunctionTrees: Seq[Tree[(Set[VI], Seq[FI])]] = junctionTreesFromOrder(
      problemStructure.factorIndices.map(fi => problemStructure.scopeOfFactor(fi).toSet -> fi),
      variableOrder)

    //1. create format for jt-creation
    //2. merge all factors of each clique into one
    val compactedTrees: Seq[Tree[(Set[VI], Seq[FI])]] = compactJTrees(rawJunctionTrees)

    val largeRegions: Map[Set[VI], Set[FI]] = compactedTrees.flatMap(_.flatten).map{
      case (vs,fs) => (vs,fs.toSet)
    }(collection.breakOut)

    def slidingTree[A](t: Tree[A]): Stream[(A,A)] = t match {
      case Tree.Node(a, leafs)    => leafs.map(l => (a,l.rootLabel)) ++ leafs.flatMap(slidingTree)
    }

    //map a small region to its large parent regions (the cliques of the junction tree)
    //note that different neighbouring large regions may have the same sepset, and we collapse the resulting
    //identically-scoped small regions into one (I assume this is correct)
    val smallRegionsWithParents: Map[Set[VI],Seq[Set[VI]]] = compactedTrees.flatMap(tree =>
      slidingTree(tree).map{case ((pa,_),(ca,_)) => (pa intersect ca,Seq(pa,ca))}
    )
      .groupBy(_._1).map{case (small,intersections) => small -> intersections.flatMap(_._2).distinct} //group identical small regions together

    new TwoLayerOC(
      problemStructure = problemStructure,
      smallRegionsScopes = smallRegionsWithParents.keySet,
      largeRegionsScopes = largeRegions.keySet,
      largeRegions.apply,
      { (l: Set[VI], s: Set[VI]) => if (smallRegionsWithParents(s).contains(l)) s else Set() }
    )
  }
}