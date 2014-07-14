package vultura.util

import java.util

import scala.annotation.tailrec
import scala.util.Random
import scalaz.Scalaz._
import scalaz.Tree._
import scalaz._

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
    maxCliqueSize
  }

  /** places the new elimination clique at the head of the result. */
  def eliminateVertex[A](cliques: Seq[Set[A]], vertex: A): (Seq[Set[A]],Int) = {
    val (cliquesWith, cliquesWithout) = cliques.partition(_.contains(vertex))
    val newClique: Set[A] = cliquesWith.view.flatten.toSet - vertex
    (newClique +: cliquesWithout, newClique.size)
  }

  def simplicialEdges[A](cliques: Seq[Set[A]], allVariables: Option[Set[A]] = None): Set[A] = {
      allVariables.getOrElse(cliques.flatten.toSet).filter{v =>
      cliques.count(_.contains(v)) == 1
    }
  }

  def eliminateSimplicials[A](cliques: Seq[Set[A]],allVariables: Option[Set[A]] = None): (Seq[Set[A]],List[A]) = {
    val simplicialIter: Iterator[(Seq[Set[A]], Seq[A])] =
      Iterator.iterate((cliques,simplicialEdges(cliques).toSeq)){case (cliqs, simplicials) =>
        val elminationResult = simplicials.foldLeft(cliqs)(eliminateVertex(_,_)._1)
        (elminationResult,simplicialEdges(elminationResult).toSeq)
      }.takeWhile(_._2 != Nil)
    simplicialIter.foldLeft((cliques,List.empty[A])){case ((_,elims),(rem,newElim)) => (rem,elims ++ newElim)}
  }

  def intSet2BS(is: Iterable[Int]): util.BitSet = {
    val result = new util.BitSet
    is.foreach(result.set(_))
    result
  }

  @tailrec
  def minDegreeOrderingAndWidthSlow(cliques: Seq[Set[Int]], acc: List[Int] = Nil, maxSize: Int = 0): (List[Int],Int) = {
    val vertices = cliques.flatten.distinct
    if (vertices.isEmpty) {
      (acc.reverse, maxSize)
    } else {
      def degree(v: Int): Int = cliques.foldLeft(Set[Int]()) {
        case (in, next) => if (next(v)) in union next else in
      }.size - 1
      val minDegreeVertex = vertices.minBy(degree)
      val (elimRest,cliqueSize) = eliminateVertex(cliques, minDegreeVertex)
      minDegreeOrderingAndWidthSlow(elimRest, minDegreeVertex :: acc, maxSize max cliqueSize)
    }
  }


  def weightedMinDegree(domainSizes: Int => Int): Seq[Set[Int]] => Int => Int = {cliques: Seq[Set[Int]] => v: Int =>
    val neighbours = cliques.foldLeft(Set.empty[Int]){case (acc, edge) => if(edge(v)) acc ++ edge else acc}
    neighbours.foldLeft(1){case (acc, nextVar) => acc * domainSizes(nextVar)}
  }

  @tailrec
  def vertexOrdering(selectVariable: Seq[Set[Int]] => Int => Int)(cliques: Seq[Set[Int]], acc: List[Int] = Nil, maxWeight: Int = 0, random: Random = new Random): (List[Int],Int) = {
    val vertices = cliques.flatten.distinct
    if (vertices.isEmpty) {
      (acc.reverse, maxWeight)
    } else {
      val selectedVertex = maxByMultiple(vertices)(v => -selectVariable(cliques)(v)).pickRandom(random)
      val (elimRest,_) = eliminateVertex(cliques, selectedVertex)
      vertexOrdering(selectVariable)(elimRest, selectedVertex :: acc, maxWeight max selectVariable(cliques)(selectedVertex))
    }
  }


  def bs2Iterator(bs: util.BitSet): Iterator[Int] = Iterator.iterate(0)(n => bs.nextSetBit(n) + 1).drop(1).map(_ - 1).takeWhile(_ >= 0)

  /** This is a rather efficient method to compute a mindegree ordering. */
  def minDegreeOrderingAndWidth(_cliques: IndexedSeq[Set[Int]]): (List[Int],Int) = {
    val cliques = _cliques map intSet2BS

    val vertices: IndexedSeq[Int] = {
      val bs = new util.BitSet
      cliques foreach (bs or _)
      bs2Iterator(bs).toIndexedSeq
    }

    val neighbours: IndexedSeq[util.BitSet] = vertices map {v =>
      val bs = new util.BitSet
      cliques filter (_ get v) foreach (bs or _)
      bs
    }

    //warning: mutability is used in here
    @tailrec
    def mdo(cliques: Seq[util.BitSet], vertsWithN: Seq[(Int,util.BitSet)], acc: (List[Int],Int) = (Nil,0)): (List[Int],Int) = {
      if(vertsWithN.isEmpty)
        (acc._1.reverse, acc._2)
      else {
        val (elimV,elimN) = vertsWithN minBy (_._2.cardinality)
        val (collectedCliques, remainingCliques) = cliques partition (_ get elimV)
        val elimClique = {
          val bs = new util.BitSet
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
        //elimClique cardinality corresponds to current treewidth because eliminated vertex is already removed
        mdo(newCliques, newVwithN, (elimV :: acc._1, acc._2 max elimClique.cardinality))
      }
    }

    mdo(cliques,vertices zip neighbours)
  }

  /** Contract edges of a tree using the `join` function. */
  def partialFoldTree[A](t: Tree[A])(join: (A,A) => Option[A]): Tree[A] = t match {
    case leaf@Node(a, sf) if sf.isEmpty => leaf
    case Node(a,subForest) =>
      val (newA, newChildren, changed) = subForest.foldLeft((a,List[Tree[A]](),false)){ case ((accA,accChildren,accChange),child@Node(childLabel,childChildren)) =>
        join(accA,childLabel).map(joinedA => (joinedA,childChildren ++: accChildren,true)).getOrElse((accA,child :: accChildren,accChange))
      }
      //do we have to process the same node again, because we pulled up some children?
      if(changed)
        partialFoldTree(node(newA,newChildren.toStream))(join)
      else
        node(newA,newChildren.map(partialFoldTree(_)(join)).toStream)
  }

  def compactJTrees[A](trees: Seq[Tree[(Set[Int],Seq[A])]]): Seq[Tree[(Set[Int],Seq[A])]] =
    trees.map(t => partialFoldTree(t){
      case ((vs1,fs1),(vs2,fs2)) if vs1 subsetOf vs2 => Some((vs2,fs1 ++ fs2))
      case ((vs1,fs1),(vs2,fs2)) if vs2 subsetOf vs1 => Some((vs1,fs1 ++ fs2))
      case _ => None
    })

  def junctionTreesFromOrder[A](cliques: Seq[(Set[Int],A)], order: Seq[Int]): Seq[Tree[(Set[Int],Seq[A])]] = {
    //the first set contains variables to propagate upwards, the second set contains those in the local clique
    //the first set does not contain the eliminated vertex, the second one does; that's the reason for the two sets
    def jtRec(leafs: Seq[Tree[(Set[Int], Set[Int], Seq[A])]], order: List[Int]): Seq[Tree[(Set[Int], Seq[A])]] = order match {
      case Nil =>
        require(leafs.forall(_.rootLabel._1.isEmpty), "given order did not eliminate all variables")
        leafs.map(tree => tree.map{case (_,c,a) => (c,a)})

      case next :: rest =>
        val (elim, remain) = leafs.partition(_.rootLabel._1.contains(next))
        val elimClique: Set[Int] = elim.map(_.rootLabel._1).flatten.toSet
        val newTree = node((elimClique - next, elimClique, Seq()), elim.toStream)
        jtRec(remain :+ newTree, rest)
    }

    jtRec(cliques.map{case (vars, a) => leaf((vars, vars, Seq(a)))}, order.toList)
  }

  def minDegreeJTs[A](_cliques: IndexedSeq[(Set[Int],A)]): Seq[Tree[(Set[Int],Seq[A])]] = {
    //convert the scala sets to BitSets
    val cliques = _cliques.map(c => intSet2BS(c._1))

    //this will be the initial trees; those bitsets are not supposed to be mutated
    //the second tuple entry is the singleton seq of "factors" (As)
    val leafs: IndexedSeq[Tree[(util.BitSet, Seq[A])]] =
      cliques.map(_.clone.asInstanceOf[util.BitSet]).zip(_cliques.map(c => Seq(c._2))).map(leaf(_))

    val vertices: IndexedSeq[Int] = {
      val bs = new util.BitSet
      cliques foreach (bs or _)
      bs2Iterator(bs).toIndexedSeq
    }

    val neighbours: IndexedSeq[util.BitSet] = vertices map {v =>
      val bs = new util.BitSet
      cliques filter (_ get v) foreach (bs or _)
      bs
    }

    /*
    - warning: mutability is used in here
    - the produced tree still needs to be compressed

    (*) The algorithm keeps track of the neighbours of every edge using the second parameter. These bitsets are mutated
    after each elimination step (remove the eliminated vertex and add new neighbours if the node was
    part of the elimination clique).

    the constructed tree has all cliques only at the leafs and empty inner nodes.
     */
    @tailrec
    def mdo(cliques: IndexedSeq[(util.BitSet,Tree[(util.BitSet,Seq[A])])], vertsWithN: Seq[(Int,util.BitSet)]): Seq[Tree[(util.BitSet,Seq[A])]] = {
      if(vertsWithN.isEmpty){
        //all cliques should be empty now; number of final cliques equals number of components of graph
        assert(cliques.forall(_._1.isEmpty))
        cliques.map(_._2)
      }
      else {
        val (elimV: Int,elimNeighbours: util.BitSet) = vertsWithN minBy (_._2.cardinality)
        val (collectedCliques, remainingCliques) = cliques partition (_._1 get elimV)
        //combine the bitset and the trees; for the tree simply take the new elimination clique as root and the trees of
        //all eliminated cliques as children
        val elimTuple = {
          val bs = new util.BitSet
          collectedCliques foreach (bs or _._1)
          val bsForTree = new util.BitSet
          bsForTree.or(bs)
          //remove the eliminated vertex after making the copy of the clique for the junction tree
          bs.clear(elimV)
          val newTree: Tree[(util.BitSet,Seq[A])] =
            node((bsForTree,Seq()), collectedCliques.map(_._2).toStream)
          (bs,newTree)
        }

        val newCliques = remainingCliques :+ elimTuple

        //update the neighbour lookup
        val newVwithN = vertsWithN filterNot (_._1 == elimV)
        //update the bitsets (see (*) in comment
        newVwithN.foreach{ case (v,ns) =>
          if (ns.get(elimV)){
            ns.or(elimNeighbours)
            ns.clear(elimV)
          }
        }
        //recurse
        mdo(newCliques, newVwithN)
      }
    }

    mdo(cliques zip leafs,vertices zip neighbours).map(_.map{case (vars,as) => (bs2Iterator(vars).toSet,as)})
  }

  /** Checks the junctiontreedness of the arguments. You need a working equals on the `A`s.
   * @return Some error message or None if everything is fine.
   */
  def isJunctionTree[A](cliques: IndexedSeq[(Set[Int],A)], trees: Seq[Tree[(Set[Int],Seq[A])]]): Option[String] = {
    val domMap: Map[A, Set[Int]] = cliques.map(_.swap).toMap
    //for tree nodes: check that first in tuple equals the union over second
    val domainViolation: Option[String] = trees.flatMap(_.flatten)
      .find{case (dom,funs) => !funs.flatMap(domMap).toSet.subsetOf(dom)}
      .map("domain of tree node is no superset of factor domains in: " + _.toString)

    //every A appears exactly once
    val countsFromReference: Map[A, Int] = cliques.map(_._2).groupBy(identity).mapValues(_.size)
    val countsFromTree: Map[A, Int] = trees.flatMap(_.map(_._2).flatten).flatten.groupBy(identity).mapValues(_.size)
    val partitionViolation = if(countsFromReference == countsFromTree)
      None
    else
      Some(
        "not a partition:\n" +
          "ref - tree: " + (countsFromReference -- countsFromTree.keys) + "\n" +
          "tree - ref: " + (countsFromTree -- countsFromReference.keys))

    //check the running intersection property
    //the variables in active may still appear, the variables in spent may not appear again
    def checkTree(t: Tree[Set[Int]], active: Set[Int] = Set()): Map[Int,Int] = {
      def addKeys(m1: Map[Int,Int], m2: Map[Int,Int]): Map[Int,Int] = m2.foldLeft(m1.withDefaultValue(0)){case (m,(k,v)) => m + (k -> (m(k) + v))}

      val label = t.rootLabel
      val newVars: Set[Int] = label -- active
      t.subForest.map(checkTree(_,label)).foldLeft(newVars.map(_ -> 1).toMap){case (m,mSub) => addKeys(m,mSub)}
    }
    //check the running intersection property: the junctions a certain variable appears in form  a subtree
    val runningIntersectionViolation = trees.map(_.map(_._1)).map(checkTree(_)).find(_.values.exists(_ != 1)).map(_ => "running intersection violated")

    //check that no variable is contained in two trees
    val allVars = cliques.map(_._1).flatten.distinct
    val containingTrees = allVars.map(v => v -> trees.filter(_.flatten.exists(_._1.contains(v))))
    val violatedVars = containingTrees.find(_._2.size != 1).map(t => "variable appears in more or less than one tree: " + (t :-> printJTs))

    val result = domainViolation orElse partitionViolation orElse runningIntersectionViolation orElse violatedVars

    result
  }

  def printJTs[A](trees: Seq[Tree[A]]): String = trees.map(_.map(_.toString).drawTree).mkString("\n---\n")

  def minDegreeOrdering(cliques: Seq[Set[Int]]): List[Int] = minDegreeOrderingAndWidth(cliques.toIndexedSeq)._1
}