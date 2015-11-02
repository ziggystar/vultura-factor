package vultura.factor.inference.gbp

import vultura.factor.ProblemStructure
import vultura.util._
import vultura.util.graph.DotGraph

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

  def edges: Set[(Region,Region)] = for{
    parent <- regions
    child <- children(parent)
  } yield parent -> child

  def issues: Seq[String] = {
    //all factors are in top regions
    val innerFactors = regions.filter(parents(_).nonEmpty) //exists child regions
      .exists(factorsOf(_).nonEmpty)                        //that has factors assigned?
    Seq(
      Some("there exist inner factors").filter(_ => innerFactors)
    ).flatten
  }

  def toDot: DotGraph[Region] = DotGraph.apply(edges)
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

case class TwoLayerRG(problemStructure: ProblemStructure,
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

case class OvercountingRegionGraph(problemStructure: ProblemStructure, regions: Set[Set[ProblemStructure#VI]], factorAssignment: Map[ProblemStructure#FI,Set[ProblemStructure#VI]])
  extends RegionGraph with ChildMapRG with OverCountingNumbers {
  type Region = Set[problemStructure.VI]

  val factorsOfRegion: Map[Region,Set[FI]] = regions.map(r => r -> factorAssignment.filter(_._2 == r).keySet).toMap.withDefaultValue(Set())


  override protected def childrenInitializer(r: Set[problemStructure.VI]): Set[Set[problemStructure.VI]] = {
    val ancs = regions.filter(_.subsetOf(r)) - r
    ancs.filter(r => !ancs.exists(anc => r.subsetOf(anc) && anc != r))
  }

  override def factorsOf(r: Set[VI]): Set[FI] = factorsOfRegion(r)
  override def variablesOf(r: Set[VI]): Set[VI] = r
}

object RegionGraph {
  def betheRG(ps: ProblemStructure): TwoLayerRG = {
    val upper: SSet[Int] = new SSet(ps.scopeOfFactor.map(_.toSet)(collection.breakOut))
    TwoLayerRG(ps,
      lowerRegions = ps.variables.map(Set(_))(collection.breakOut),
      upperRegions = upper.maximalSets,
      factorAssignment = ps.factorIndices.map(fi => fi -> upper.maximalSuperSetsOf(ps.scopeOfFactor(fi).toSet).head)(collection.breakOut)
    )
  }
}
