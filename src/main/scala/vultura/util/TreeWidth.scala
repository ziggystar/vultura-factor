package vultura.util

import java.util.BitSet
import collection.Seq
import annotation.tailrec

/**
 * @author Thomas Geier
 * @since 12.02.12
 */

object TreeWidth {
  def treeWidth[A](cliques: Seq[Set[A]], ordering: Seq[A]): Int = {
    val (elimination,maxCliqueSize) = ordering.foldLeft((cliques,0)){case ((remCliques,max),elimVar) =>
      val elimResult = eliminateVertex(remCliques,elimVar)
      (elimResult,math.max(max,elimResult.head.size))
    }
    assert(elimination.flatten.size == 0, "there was something left after eliminating everything")
    maxCliqueSize - 1
  }

  /** places the new elimination clique at the head of the result. */
  def eliminateVertex[A](cliques: Seq[Set[A]], vertex: A): Seq[Set[A]] = {
    val (cliquesWith, cliquesWithout) = cliques.partition(_.contains(vertex))
    val newClique: Set[A] = cliquesWith.view.flatten.toSet - vertex
    newClique +: cliquesWithout
  }

  def simplicialEdges[A](cliques: Seq[Set[A]], allVariables: Option[Set[A]] = None): Set[A] = {
      allVariables.getOrElse(cliques.flatten.toSet).filter{v =>
      cliques.filter(_.contains(v)).size == 1
    }
  }

  def eliminateSimplicials[A](cliques: Seq[Set[A]],allVariables: Option[Set[A]] = None): (Seq[Set[A]],List[A]) = {
    val simplicialIter: Iterator[(Seq[Set[A]], Seq[A])] =
      Iterator.iterate((cliques,simplicialEdges((cliques)).toSeq)){case (cliques, simplicials) =>
        val elminationResult = simplicials.foldLeft(cliques)(eliminateVertex(_,_))
        (elminationResult,simplicialEdges(elminationResult).toSeq)
      }.takeWhile(_._2 != Nil)
    simplicialIter.foldLeft((cliques,List.empty[A])){case ((_,elims),(rem,newElim)) => (rem,elims ++ newElim)}
  }

  def intSet2BS(is: Iterable[Int]): BitSet = {
    val result = new BitSet
    is.foreach(result.set)
    result
  }

  @tailrec
  def minDegreeOrdering(cliques: Seq[Set[Int]], acc: List[Int] = Nil): List[Int] = {
    val vertices = cliques.flatten.distinct
    if (vertices.isEmpty) {
      acc.reverse
    } else {
      def degree(v: Int): Int = cliques.foldLeft(Set[Int]()) {
        case (in, next) => if (next(v)) in union next else in
      }.size - 1
      val minDegreeVertex = vertices.minBy(degree)
      minDegreeOrdering(eliminateVertex(cliques, minDegreeVertex), minDegreeVertex :: acc)
    }
  }
}