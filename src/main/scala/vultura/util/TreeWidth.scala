package vultura.util

import annotation.tailrec

/**
 * @author Thomas Geier
 * @since 12.02.12
 */

object TreeWidth {
  @tailrec
  def treeWidth[A](cliques: List[Set[A]], ordering: List[A], acc: Int = 0): Int = ordering match {
    case Nil => acc
    case h :: t => {
      val (cliquesWith: List[Set[A]], cliquesWithout) = cliques.partition(_.contains(h))
      val newClique: Set[A] = cliquesWith.view.flatten.foldLeft(Set.newBuilder[A])(_ += _).result() - h
      treeWidth(newClique :: cliquesWithout, t, math.max(acc,newClique.size))
    }
  }
}