package vultura.graph

import scalaz._
import Scalaz._
/**
 * Type class for trees.
 * User: Thomas Geier
 * Date: 24.10.11
 */

object TreeAsGraph {
  implicit def treeAsGraph[A]: Graph[Tree[A],A] = new Graph[Tree[A],A] {
    def nodes(g: Tree[A]): Set[A] = g.flatten.toSet
    def adjacent(g: Tree[A], n1: A, n2: A): Boolean = {
      val childLabeledTree: Tree[(A, Stream[A])] = g =>> (t => t.rootLabel -> t.subForest.map(_.rootLabel))
      childLabeledTree.flatten.exists(tt => tt._2.contains(tt._1))
    }
  }
}
