package vultura.inference.gbp

import vultura.factor.ProblemStructure
import vultura.util.SSet
import vultura.util.graph.Tree

/** Two-layered region graph with overcounting numbers.
 *
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
    val variableOrder: Seq[Int] = treeDecomposition(problemStructure.scopeOfFactor.filterNot(_.isEmpty), problemStructure.domains)
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

    val largeRegions: Seq[(Set[VI], Set[FI])] = compactedTrees.flatMap(_.flatten).map{ case (vs,fs) => (vs,fs.toSet) }
    val largeRegionsMap: Map[Set[VI], Set[FI]] = {
      val (voids: Seq[(Set[VI], Set[FI])], rest) = largeRegions.partition(_._1 == Set())
       rest.toMap + (Set[Int]() -> voids.flatMap(_._2).toSet)
    }

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
      largeRegionsScopes = largeRegionsMap.keySet,
      largeRegionsMap.apply,
      { (l: Set[VI], s: Set[VI]) => if (smallRegionsWithParents(s).contains(l)) s else Set() }
    )
  }
}