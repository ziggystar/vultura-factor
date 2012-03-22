package vultura.util

import java.util.BitSet
import annotation.tailrec
import vultura.graph.{Graph, GraphOps}

/**
 * @author Thomas Geier
 * @since 12.02.12
 */

object TreeWidth {
  def treeWidth[A](cliques: Seq[Set[A]], ordering: Seq[A]): Int = {
    val (elimination,maxCliqueSize) = ordering.foldLeft((cliques,0)){case ((remCliques,max),elimVar) =>
      val elimResult = eliminateVertex(remCliques,elimVar)._1
      (elimResult,math.max(max,elimResult.head.size))
    }
    assert(elimination.flatten.size == 0, "there was something left after eliminating everything")
    maxCliqueSize - 1
  }

  /** places the new elimination clique at the head of the result. */
  def eliminateVertex[A](cliques: Seq[Set[A]], vertex: A): (Seq[Set[A]],Int) = {
    val (cliquesWith, cliquesWithout) = cliques.partition(_.contains(vertex))
    val newClique: Set[A] = cliquesWith.view.flatten.toSet - vertex
    (newClique +: cliquesWithout, newClique.size)
  }

  def simplicialEdges[A](cliques: Seq[Set[A]], allVariables: Option[Set[A]] = None): Set[A] = {
      allVariables.getOrElse(cliques.flatten.toSet).filter{v =>
      cliques.filter(_.contains(v)).size == 1
    }
  }

  def eliminateSimplicials[A](cliques: Seq[Set[A]],allVariables: Option[Set[A]] = None): (Seq[Set[A]],List[A]) = {
    val simplicialIter: Iterator[(Seq[Set[A]], Seq[A])] =
      Iterator.iterate((cliques,simplicialEdges((cliques)).toSeq)){case (cliqs, simplicials) =>
        val elminationResult = simplicials.foldLeft(cliqs)(eliminateVertex(_,_)._1)
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
  def minDegreeOrderingAndWidth(cliques: Seq[Set[Int]], acc: List[Int] = Nil, maxSize: Int = 0): (List[Int],Int) = {
    val vertices = cliques.flatten.distinct
    if (vertices.isEmpty) {
      (acc.reverse, maxSize)
    } else {
      def degree(v: Int): Int = cliques.foldLeft(Set[Int]()) {
        case (in, next) => if (next(v)) in union next else in
      }.size - 1
      val minDegreeVertex = vertices.minBy(degree)
      val (elimRest,cliqueSize) = eliminateVertex(cliques, minDegreeVertex)
      minDegreeOrderingAndWidth(elimRest, minDegreeVertex :: acc, (maxSize max cliqueSize))
    }
  }


  def bs2Iterator(bs: BitSet): Iterator[Int] = Iterator.iterate(0)(n => bs.nextSetBit(n) + 1).drop(1).map(_ - 1).takeWhile(_ >= 0)

  def minDegreeOrderingAndWidthFast(_cliques: Seq[Set[Int]]): (List[Int],Int) = {
    val cliques = _cliques map intSet2BS

    val vertices: IndexedSeq[Int] = {
      val bs = new BitSet
      cliques foreach (bs or _)
      bs2Iterator(bs).toIndexedSeq
    }

    val neighbours: IndexedSeq[BitSet] = vertices map {v =>
      val bs = new BitSet
      cliques filter (_ get v) foreach (bs or _)
      bs
    }

    //warning: mutability is used in here
    @tailrec
    def mdo(cliques: Seq[BitSet], vertsWithN: Seq[(Int,BitSet)], acc: (List[Int],Int) = (Nil,0)): (List[Int],Int) = {
      if(vertsWithN.isEmpty)
        (acc._1.reverse, acc._2)
      else {
        val (elimV,elimN) = vertsWithN minBy (_._2.cardinality)
        val (collectedCliques, remainingCliques) = cliques partition (_ get elimV)
        val elimClique = {
          val bs = new BitSet
          collectedCliques foreach (bs or _)
          bs.clear(elimV)
          bs
        }

        val newCliques = remainingCliques :+ elimClique
        val newVwithN = vertsWithN filterNot (_._1 == elimV)
        //update the bitsets
        newVwithN.foreach{ case (v,ns) =>
          if (ns.get(elimV)){
            ns.or(elimN)
            ns.clear(elimV)
          }
        }
        //recurse
        mdo(newCliques, newVwithN, (elimV :: acc._1, acc._2 max elimClique.cardinality))
      }
    }

    import scalaz._
    import Scalaz._
    mdo(cliques,vertices zip neighbours) :-> ((_:Int) - 1)
  }

  def minDegreeOrdering(cliques: Seq[Set[Int]]): List[Int] = minDegreeOrderingAndWidthFast(cliques)._1//minDegreeOrderingAndWidth(cliques)._1

  def libtw(_cliques: Seq[Set[Int]]): Int = {
    val cls: Seq[BitSet] = _cliques map intSet2BS
    implicit val cls2graph = new Graph[Seq[BitSet],Int] {
      def nodes(g: Seq[BitSet]): Set[Int] = {
        val bs = new BitSet
        g foreach (bs or _)
        bs2Iterator(bs).toSet
      }

      def adjacent(g: Seq[BitSet], n1: Int, n2: Int): Boolean = g.exists(c => c.get(n1) && c.get(n2))
    }
    GraphOps.treeWidth(cls)
  }
}