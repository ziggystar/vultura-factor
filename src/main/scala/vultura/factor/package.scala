package vultura

import vultura.util.TreeWidth

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 2/13/13
 */
package object factor {
  type Var = Int
  type Val = Int

  def variableElimination(p: Problem): Double = {
    val Problem(problem: IndexedSeq[Factor], domains: Array[Int], ring: Ring[Double]) = p
    val graph: IndexedSeq[Set[Int]] = problem.map(_.variables.toSet)
//    val (ordering: List[Int], potentialSize) = TreeWidth.vertexOrdering(TreeWidth.weightedMinDegree(domains))(graph)
    val (ordering: List[Int], _) = TreeWidth.minDegreeOrderingAndWidth(graph)

    val eliminationResult: List[Factor] = ordering.foldLeft(problem.toList) {
      case (factors, elimVar) =>
        val (eliminatedFactors, remainingFactors) = factors.partition(_.variables.contains(elimVar))
        val product = Factor.multiplyMarginalize(ring)(domains)(eliminatedFactors, Array(elimVar))
        product :: remainingFactors
    }
    val constants: Array[Double] = eliminationResult.map(_.values.head)(collection.breakOut)
    ring.prodA(constants)
  }
}
