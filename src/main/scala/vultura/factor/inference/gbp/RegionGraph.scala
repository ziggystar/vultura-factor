package vultura.factor.inference.gbp

import vultura.factor.ProblemStructure
import vultura.util._
import vultura.util.graph.LabeledGraph
import vultura.util.graph.graphviz.DotGraph

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

@deprecated("thesis cleanup")
trait RegionGraph {
  def problemStructure: ProblemStructure

  type Region

  type VI = ProblemStructure#VI
  type FI = ProblemStructure#FI

  def variablesOf(r: Region): Set[VI]
  def factorsOf(r: Region): Set[FI]
  def weightOf(r: Region): Double

  def regionsContaining(scope: Set[VI]): Set[Region] = regions.filter(scope subsetOf variablesOf(_))
  def regionOfFactor(fi: FI): Region = regions.find(r => factorsOf(r).contains(fi)).get
  def regions: Set[Region]
  /** Direct predecessors of `r`. */
  def parents(r: Region): Set[Region]
  /** Direct successors of `r`. */
  def children(r: Region): Set[Region]
  /** Transitive closure of `parents` for `r`. */
  def ancestors(r: Region): Set[Region]
  /** Transitive closure of `children` for `r`. */
  def descendants(r: Region): Set[Region]

  def outerRegions: Set[Region] = regions.filter(r => parents(r).isEmpty)
  def innerRegions: Set[Region] = regions -- outerRegions
  /** All descendants and the region itself. */
  def interior(r: Region): Set[Region] = descendants(r) + r
  /** All regions that are parent to an interior region of `r`, but are not interior themselves. */
  def boundary(r: Region): Set[Region] = interior(r).flatMap(parents) -- interior(r)

  def edges: Set[(Region,Region)] = for{
    parent <- regions
    child <- children(parent)
  } yield parent -> child


  def directedGraph: LabeledGraph[Region] = LabeledGraph.fromChildList(regions,r => children(r))

  def toDot: DotGraph[Region, (Region,Region)] = DotGraph.apply(regions,edges).labelNodes{case r => weightOf(r).toString}

  object Diagnosis {
    lazy val validityChecks: Seq[(String,() => Boolean)] = Seq(
      "variables are connected" -> (() => variablesAreConnected),
      "factors are connected" -> (() => factorsAreConnected),
      "weights of regions containing a variable don't sum to 1" -> (() => variablesAreCountedOnce),
      "weights of regions containing a factor don't sum to 1" -> (() => factorsAreCountedOnce)
    )

    lazy val validityIssues: Seq[String] = validityChecks.collect{case (msg,v) if !v() => msg}

    lazy val isValidRegionGraph = validityIssues.isEmpty

    /** Redundancy means that each variable-induced sub-graph is a tree. */
    def nonRedundantVariables: IndexedSeq[VI] = problemStructure.variables
      .filterNot(v => directedGraph.filterNodes(regionsContaining(Set(v))).isTree)
    def isNonRedundant: Boolean = nonRedundantVariables.nonEmpty

    def variablesAreCountedOnce: Boolean = problemStructure.variables
      .exists(vi => math.abs(regionsContaining(Set(vi)).map(weightOf).sum - 1d) > 1e-7)

    def factorsAreCountedOnce: Boolean = problemStructure.factorIndices
      .forall(fi => math.abs(weightOf(regionOfFactor(fi)) - 1d) < 1e-7)

    def factorsAreInOuterRegions: Boolean = regions.exists(r => parents(r).nonEmpty && factorsOf(r).nonEmpty)

    def variablesAreConnected: Boolean = problemStructure.variables
      .forall(v => directedGraph.filterNodes(regionsContaining(Set(v))).connectedComponents.size == 1)
    def factorsAreConnected: Boolean = problemStructure.factorIndices
      .forall{fi => directedGraph.filterNodes(regionsContaining(problemStructure.scopeOfFactor(fi).toSet)).connectedComponents.size == 1 }
  }
}

@deprecated("thesis cleanup")
object RegionGraph {
  def betheRG(ps: ProblemStructure): JunctionGraph = {
    val upper: SSet[Int] = new SSet(ps.scopeOfFactor.map(_.toSet)(collection.breakOut))
    JunctionGraph(ps,
      lowerRegions = ps.variables.map(Set(_))(collection.breakOut),
      upperRegions = upper.maximalSets,
      factorAssignment = ps.factorIndices.map(fi => fi -> upper.maximalSuperSetsOf(ps.scopeOfFactor(fi).toSet).head)(collection.breakOut)
    )
  }
}

/** Notation from Wang et Zhou: "Simplifying Generalized Belief Propagation...". */
@deprecated("thesis cleanup")
object WangZhouNotation {
  trait WZRegionGraph {
    type R
    def <(r1: R, r2: R): Boolean
    def >(r1: R, r2: R): Boolean
    def <=(r1: R, r2: R): Boolean  = <(r1,r2) || r1 == r2
    def >=(r1: R, r2: R): Boolean = >(r1,r2) || r1 == r2
    def all_<(r: R): Set[R]
    def all_>(r: R): Set[R]
    def all_<=(r: R): Set[R]
    def all_>=(r: R): Set[R]
    /** Return all regions in the boundary of `r`. */
    def B(r: R): Set[R]
    def I(r: R): Set[R] = all_<=(r)
    def A(r: R): Set[R] = all_>(r)
  }

  implicit class WZRegionGraphWrapper(val rg: RegionGraph) extends WZRegionGraph {
    type R = rg.Region
    def <(r1: R, r2: R): Boolean = rg.descendants(r2).contains(r1)
    def >(r1: R, r2: R): Boolean = rg.descendants(r1).contains(r2)
    def all_<(r: R): Set[R] = rg.descendants(r)
    def all_>(r: R): Set[R] = rg.ancestors(r)
    def all_<=(r: R): Set[R] = rg.descendants(r) + r
    def all_>=(r: R): Set[R] = rg.ancestors(r) + r
    def B(r: R): Set[R] = rg.boundary(r)
  }
}

/** Somewhat efficient implementation of region graph methods. */
@deprecated("thesis cleanup")
trait ChildMapRG { self: RegionGraph =>
  protected def childrenInitializer(r: Region): Set[Region]
  protected lazy val childMap: Map[Region, Set[Region]] =
    (regions.map(r => r -> childrenInitializer(r))(collection.breakOut): HashMap[Region, Set[Region]]).withDefaultValue(Set())
  protected lazy val descendantMap: Map[Region, Set[Region]] = transitiveClosure(childMap).withDefaultValue(Set())
  protected lazy val parentMap =  reverseMultiMap(childMap).withDefaultValue(Set())
  protected lazy val parentMapTrans = transitiveClosure(parentMap).withDefaultValue(Set())

  final override def children(r: Region): Set[Region] = childMap(r)
  final override def descendants(r: Region): Set[Region] = descendantMap(r)
  final override def ancestors(r: Region): Set[Region] = parentMapTrans(r)
  final override def parents(r: Region): Set[Region] = parentMap(r)
}

@deprecated("thesis cleanup")
trait OverCountingNumbers {self: RegionGraph =>
  protected lazy val overCountingNumbers: Map[Region,Int] = {
    require(regions.nonEmpty, "cannot compute overcounting numbers for empty region graph")
    val topo = {
      def findSource(remaining: Set[Region]): Option[Region] = remaining.find(cand => ancestors(cand).intersect(remaining).isEmpty)
      @tailrec def topoR(acc: List[Region], remaining: Set[Region]): Seq[Region] = if(remaining.isEmpty) acc.reverse else {
        val next = findSource(remaining).getOrElse(throw new RuntimeException("region graph is cyclic"))
        topoR(next::acc, remaining - next)
      }
      topoR(Nil,regions)
    }
    topo.foldLeft(Map[Region,Int]()){case (m,r) => m + (r -> (1 - ancestors(r).foldLeft(0)(_ + m(_))))}
  }
  final override def weightOf(r: Region): Double = overCountingNumbers(r).toDouble
}
