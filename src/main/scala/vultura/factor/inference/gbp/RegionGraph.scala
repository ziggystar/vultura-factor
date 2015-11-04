package vultura.factor.inference.gbp

import vultura.factor.ProblemStructure
import vultura.util._
import vultura.util.graph.DotGraph
import vultura.util.graph2._

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

trait RegionGraph {
  def problemStructure: ProblemStructure

  type Region

  type VI = ProblemStructure#VI
  type FI = ProblemStructure#FI

  def variablesOf(r: Region): Set[VI]
  def factorsOf(r: Region): Set[FI]
  def weightOf(r: Region): Double

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
  def interior(r: Region): Set[Region] = ancestors(r) + r
  /** All regions that are parent to an interior region of `r`, but are not interior themselves. */
  def boundary(r: Region): Set[Region] = interior(r).flatMap(parents) -- interior(r)

  def edges: Set[(Region,Region)] = for{
    parent <- regions
    child <- children(parent)
  } yield parent -> child

  def issues: Seq[String] = for{
    (issueDesc,details) <- Seq(
      "region graph is non-redundant" -> isNonRedundant,
      "variables counting numbers don't sum to one" -> variablesAreCountedOnce,
      "factors counting numbers don't sum to one" -> factorsAreCountedOnce,
      "some factors are in inner regions" -> factorsAreInOuterRegions,
      "not all variables induce a connected subgraph" -> variablesAreConnected
    )
    if details.nonEmpty
  } yield issueDesc + "\n * " + details.mkString("\n * ")

  def isNonRedundant: Seq[String] = ???
  def variablesAreCountedOnce: Seq[String] = ???
  def factorsAreCountedOnce: Seq[String] = ???
  def variablesAreConnected: Seq[String] = ???
  def factorsAreInOuterRegions: Seq[String] =
    regions.toSeq.filter(parents(_).nonEmpty)               //exists child regions
      .filter(factorsOf(_).nonEmpty).map(r => s"region $r") //that has factors assigned?

  def toDot: DotGraph[Region] = DotGraph.apply(edges)
}

object RegionGraph {
  def betheRG(ps: ProblemStructure): JunctionGraph = {
    val upper: SSet[Int] = new SSet(ps.scopeOfFactor.map(_.toSet)(collection.breakOut))
    JunctionGraph(ps,
      lowerRegions = ps.variables.map(Set(_))(collection.breakOut),
      upperRegions = upper.maximalSets,
      factorAssignment = ps.factorIndices.map(fi => fi -> upper.maximalSuperSetsOf(ps.scopeOfFactor(fi).toSet).head)(collection.breakOut)
    )
  }

  /** Declares that a RegionGraph is a directed graph with regions as nodes. */
  implicit def regionGraphIsDirectedGraphInstance[R,RG <: RegionGraph{type Region = R}]: IsDirectedGraph[RG, R] =
    new IsDirectedGraph[RG,R]{
      override def nodes(x: RG): Set[R] = x.regions
      override def edges(x: RG): Set[(R, R)] = x.edges
    }
}

/** Notation from Wang et Zhou: "Simplifying Generalized Belief Propagation...". */
object WangZhouNotation {
  implicit class WZRegionGraph(val rg: RegionGraph) extends AnyVal {
    type R = rg.Region
    def <(r1: R, r2: R): Boolean = rg.descendants(r2).contains(r1)
    def >(r1: R, r2: R): Boolean = rg.descendants(r1).contains(r2)
    def <=(r1: R, r2: R): Boolean = <(r1,r2) || r1 == r2
    def >=(r1: R, r2: R): Boolean = >(r1,r2) || r1 == r2
    def all_<(r: R): Set[R] = rg.descendants(r)
    def all_>(r: R): Set[R] = rg.ancestors(r)
    def all_<=(r: R): Set[R] = rg.descendants(r) + r
    def all_>=(r: R): Set[R] = rg.ancestors(r) + r
    def I(r: R): Set[R] = all_<=(r)
    def A(r: R): Set[R] = all_>(r)
    def B(r: R): Set[R] = rg.boundary(r)
  }
}

/** Somewhat efficient implementation of region graph methods. */
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
