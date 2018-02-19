package vultura.inference.gbp

import scala.collection.immutable.IndexedSeq

/** This class provides some checks, whether a RegionGraph implements all laws. These can be expensive. */
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
