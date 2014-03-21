package vultura

import vultura.util.TreeWidth

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 2/13/13
 */
package object fastfactors {
  type Var = Int
  type Val = Int

  def veJunctionTree(p: Problem): Double = {
    import TreeWidth._
    import scalaz.Tree
    import scalaz.Tree._

    val Problem(problem: IndexedSeq[FastFactor], domains: Array[Int], ring: RingZ[Double]) = p
    val trees: Seq[Tree[(Set[Int], Seq[FastFactor])]] = compactJTrees(minDegreeJTs(problem.map(f => f.variables.toSet -> f)))
    def veR(tree: Tree[(Set[Int], Seq[FastFactor])]): (Set[Int], Seq[FastFactor]) = tree match {
      case Node((vars,factors), sf) if !sf.isEmpty =>
        (vars,factors ++ sf.map(veR(_)).map(fs => FastFactor.multiplyMarginalize(ring)(domains)(fs._2,(fs._1 -- vars).toArray)))
      case Node(root, _) => root
    }
    ring.prodA(
      trees
        .map(veR)
        .map{case (vars,factors) => FastFactor.multiplyMarginalize(ring)(domains)(factors,vars.toArray).values(0)}(collection.breakOut)
    )
  }

  def variableElimination(p: Problem): Double = {
    val Problem(problem: IndexedSeq[FastFactor], domains: Array[Int], ring: RingZ[Double]) = p
    val graph: IndexedSeq[Set[Int]] = problem.map(_.variables.toSet)
//    val (ordering: List[Int], potentialSize) = TreeWidth.vertexOrdering(TreeWidth.weightedMinDegree(domains))(graph)
    val (ordering: List[Int], _) = TreeWidth.minDegreeOrderingAndWidth(graph)

    val eliminationResult: List[FastFactor] = ordering.foldLeft(problem.toList) {
      case (factors, elimVar) =>
        val (eliminatedFactors, remainingFactors) = factors.partition(_.variables.exists(_ == elimVar))
        val product = FastFactor.multiplyMarginalize(ring)(domains)(eliminatedFactors, Array(elimVar))
        product :: remainingFactors
    }
    val constants: Array[Double] = eliminationResult.map(_.values.head)(collection.breakOut)
    ring.prodA(constants)
  }
}
