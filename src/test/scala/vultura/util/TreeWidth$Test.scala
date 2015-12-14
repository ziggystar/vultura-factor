package vultura.util

import org.specs2.mutable.Specification
import TreeWidth._
import vultura.util.graph.Tree

import scala.util.Random

/**
  * Created by thomas on 14.12.15.
  */
class TreeWidth$Test extends Specification {

  def makeJunctionTrees(cliques: Iterable[Iterable[Int]]): Seq[Tree[Set[Int]]] = {
    val allValues = cliques.flatten.toSeq.distinct
    val cliquesAA: AA[Int] = cliques.map(_.toArray).toArray
    val order = treeDecomposition(cliquesAA,allValues.map(_ => 2).toArray)
    junctionTreesFromOrder(cliques.map(c => (c.toSet,())).toSeq,order.get).map(_.map(_._1))
  }

  def treeWidth(trees: Seq[Tree[Set[Int]]]): Int = trees.flatMap(_.flatten.map(_.size)).max + 1

  def randomTree(nodes: Int, cliqueSize: Int, seed: Long = 0): Seq[Seq[Int]] = {
    val r = new Random(seed)
    Iterator.iterate(Seq[Seq[Int]]()){ t =>
      val maxNode = t.flatten.foldLeft(0)(_ max _)
      val connection = r.nextInt(maxNode + 1)
      val newClique = (1 to (cliqueSize-1)).map(_ + maxNode)
      t :+ (newClique :+ connection)
    }.drop(nodes).next()
  }

  def treeTW(nodes: Int, cliqueSize: Int): Int = treeWidth(makeJunctionTrees(randomTree(nodes,cliqueSize)))

  "make tree decomposition of a tree must not crash" >> {
    treeDecomposition(Array(Array(0,1),Array(1,2)),Array(2,2,2)) must beSome
  }

  "tree dec of random tree with 10 nodes, cliques size 2 has tw 3" >> {treeTW(10,2) === 3}
  "tree dec of random tree with 100 nodes, cliques size 2 has tw 3" >> {treeTW(100,2) === 3}
  "tree dec of random tree with 1000 nodes, cliques size 2 has tw 3" >> {treeTW(1000,2) === 3}

  "tree dec of random tree with 10 nodes, cliques size 3 has tw 4" >> {treeTW(10,3) === 4}
  "tree dec of random tree with 100 nodes, cliques size 3 has tw 4" >> {treeTW(100,3) === 4}
  "tree dec of random tree with 1000 nodes, cliques size 3 has tw 4" >> {treeTW(1000,3) === 4}
}
