package vultura.factor.inference.gbp

import vultura.factor.ProblemStructure

/**
 * New attempt at region graph.
 */
case class Reg(variables: Set[Int], factors: Set[Int], countingNumber: Int)

trait RG {
  type VI = Int
  type FI = Int

  def problemStructure: ProblemStructure
  def regions: Set[Reg]
  def parents(r: Reg): Set[Reg]
  def children(r: Reg): Set[Reg]
  def ancestors(r: Reg): Set[Reg]
  def descendants(r: Reg): Set[Reg]
  def outerRegions: Set[Reg] = regions.filter(r => parents(r).isEmpty)
  def innerRegions: Set[Reg] = regions -- outerRegions
}

case class MapRG(problemStructure: ProblemStructure, regions: Set[Reg], childMap: Map[Reg,Set[Reg]]) extends RG {
  val tranChildMap: Map[Reg, Set[Reg]] = vultura.util.transitiveClosure(childMap).withDefaultValue(Set())
  val parentMap: Map[Reg, Set[Reg]] = vultura.util.reverseMultiMap(childMap).withDefaultValue(Set())
  val tranParentMap: Map[Reg, Set[Reg]] = vultura.util.transitiveClosure(parentMap).withDefaultValue(Set())
  override def parents(r: Reg): Set[Reg] = parentMap(r)
  override def children(r: Reg): Set[Reg] = childMap(r)
  override def descendants(r: Reg): Set[Reg] = tranChildMap(r)
  override def ancestors(r: Reg): Set[Reg] = tranParentMap(r)
}

object RG {
  def betheRG(ps: ProblemStructure): MapRG = {
    val factorRegions = (0 until ps.numFactors).map(fi => Reg(ps.scopeOfFactor(fi).toSet,Set(fi),1))
    val variableRegions: Map[Int,Reg] =
      (0 until ps.numVariables).map(vi => vi -> Reg(Set(vi),Set(),1 - ps.degrees(vi)))(collection.breakOut)
    val regions: Set[Reg] = (factorRegions ++ variableRegions.values)(collection.breakOut)
    val children: Map[Reg,Set[Reg]] = factorRegions.map{case r@Reg(vs,_,_) => r -> vs.map(variableRegions)}.toMap.withDefaultValue(Set())
    MapRG(ps, regions, children)
  }
}
