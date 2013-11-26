package vultura.fastfactors.algorithms.gbp

import vultura.fastfactors.FastFactor
import scala.collection.mutable

/**
 * Stores a state of region graph propagation, i.e. the values of the messages.
 */
class ParentToChildCalibration(rg: RegionGraph) {
  /** The messages are factor with scope of the child region, going from a parent to a child. */
  val messages: mutable.HashMap[(Region,Region),FastFactor] = rg.edges.map{case edge@(parent,child) =>
    edge -> FastFactor.maxEntropy(child.variables.toArray.sorted,rg.problem.domains,rg.problem.ring)
  }(collection.breakOut)
}
