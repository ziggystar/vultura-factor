package vultura.factor.inference.gbp

import vultura.factor.{ProblemStructure, Factor, Var}

/**
 * Message-passing algorithm for constrained minimization of a convex Kikuchi free energy.
 * Publications:
 *  - "Efficient Minimization of the Kikuchi Free Energy", Tom Heskes, JAIR Vol. 26, 2006.
 */
case class ConvergentMP(rg: RG) {


}

case class Region2(variables: Set[Var], factors: Seq[Int], cr: Int)

class RG(val structure: ProblemStructure, val regions: Set[Region2], _childrenOf: Map[Region2,Set[Region2]]){
  val childrenOf: Map[Region2, Set[Region2]] = _childrenOf.withDefaultValue(Set())
  val parentsOf: Map[Region2,Set[Region2]] = regions.map(c => c -> regions.filter(n => childrenOf(n).contains(c)))(collection.breakOut)
  val (innerRegions,outerRegions) = regions.partition(r => parentsOf(r).nonEmpty)
}