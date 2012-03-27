package vultura.util

import java.util.BitSet
import annotation.tailrec
import vultura.graph.{Graph, GraphOps}
import scalaz._
import Scalaz._
import collection.mutable.HashSet
import collection.immutable.Set

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
      minDegreeOrderingAndWidthSlow(elimRest, minDegreeVertex :: acc, (maxSize max cliqueSize))
    }
  }


  def bs2Iterator(bs: BitSet): Iterator[Int] = Iterator.iterate(0)(n => bs.nextSetBit(n) + 1).drop(1).map(_ - 1).takeWhile(_ >= 0)

  /** This is a rather efficient method to compute a mindegree ordering. */
  def minDegreeOrderingAndWidth(_cliques: IndexedSeq[Set[Int]]): (List[Int],Int) = {
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

  def minDegreeJunctionTrees[A](_cliques: IndexedSeq[(Set[Int],A)]): (Seq[Tree[(Set[Int],Seq[A])]],Int) = {
    //convert the scala sets to BitSets
    val cliques = _cliques.map(c => intSet2BS(c._1))
    //this will be the initial trees; those bitsets are not supposed to be mutated
    //the second tuple entry is the singleton seq of "factors" (As)
    val leafs: IndexedSeq[Tree[(BitSet, Seq[A])]] =
      cliques.map(_.clone.asInstanceOf[BitSet]).zip(_cliques.map(c => Seq(c._2))).map(leaf(_))

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

    /*
    - warning: mutability is used in here
    - the produced tree still needs to be compressed

    (*) The algorithm keeps track of the neighbours of every edge using the second parameter. These bitsets are mutated
    after each elimination step (remove the eliminated vertex and add new neighbours if the node was
    part of the elimination clique).

    the constructed tree has all cliques only at the leafs and empty inner nodes.
     */
    @tailrec
    def mdo(cliques: IndexedSeq[(BitSet,Tree[(BitSet,Seq[A])])], vertsWithN: Seq[(Int,BitSet)], tw: Int = 0): (Seq[Tree[(BitSet,Seq[A])]],Int) = {
      if(vertsWithN.isEmpty){
        //all cliques should be empty now; number of final cliques equals number of components of graph
        assert(cliques.forall(_._1.isEmpty))
        (cliques.map(_._2),tw)
      }
      else {
        val (elimV: Int,elimNeighbours: BitSet) = vertsWithN minBy (_._2.cardinality)
        val (collectedCliques, remainingCliques) = cliques partition (_._1 get elimV)
        //combine the bitset and the trees, for the tree simply take the new elimination clique as root and the trees of
        //all eliminated cliques as children
        val elimTuple@(elimClique,elimTree) = {
          val bs = new BitSet
          collectedCliques foreach (bs or _._1)
          bs.clear(elimV)
          val newTree: Tree[(BitSet,Seq[A])] = node((bs.clone.asInstanceOf[BitSet],Seq()), collectedCliques.map(_._2).toStream)
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
        mdo(newCliques, newVwithN, tw max elimClique.cardinality)
      }
    }

    //the tree we obtain has all cliques at the leafs
    val (uncompressedJTs,tw) = mdo(cliques zip leafs,vertices zip neighbours) :-> ((_:Int) - 1)

    def subsumes(b1: BitSet, b2: BitSet): Boolean = {
      val clone = b2.clone.asInstanceOf[BitSet]
      clone.andNot(b1)
      clone.isEmpty
    }

    def scanUp[S,T](tree: Tree[S])(f:(S,Stream[Tree[T]]) => Tree[T]): Tree[T] = f(tree.rootLabel, tree.subForest.map(scanUp(_)(f)))

    //we apply two optimizations to the tree
    //first, we pull a child up, if its scope is a subset of its parents scope
    def pullUp(tree: Tree[(BitSet,Seq[A])]): Tree[(BitSet,Seq[A])] = {
      //if we pull up a processed child, we can be sure that we cannot pull the child's children up, because:
      //if a childchild is a subset of the child, it would have been pulled up
      //if a childchild is not a subset of the child, it has to contain a variable that's not in the child; this variable
      // cannot be in the parent because of the running intersection property and thus we
      // cannot pull the childchild into the parent. qed
      scanUp[(BitSet,Seq[A]),(BitSet,Seq[A])](tree){ case ((bs,as),processedChildren) =>
        val (toPull,toLeave) = processedChildren.partition(child => subsumes(bs,child.rootLabel._1))
        node((bs,as ++ toPull.map(_.rootLabel._2).flatten),toLeave ++ toPull.flatMap(_.subForest))
      }
    }
    val pulledUpJTs: Seq[Tree[(BitSet,Seq[A])]] = uncompressedJTs.map(pullUp)

    //second, we push parents down into their child if they only have one child. In this constellation, the parent's scope is
    //a subset of the child's scope.
    def pushDown(tree: Tree[(BitSet,Seq[A])]): Tree[(BitSet,Seq[A])] = if(tree.subForest.size == 1){
      assert(tree.rootLabel._2.isEmpty)
      pushDown(tree.subForest.head)
    }
    else
      node(tree.rootLabel, tree.subForest.map(pushDown))

    val pushedDownJTs: Seq[Tree[(BitSet,Seq[A])]] = pulledUpJTs.map(pushDown)

//    assert(pushedDownJTs == pushedDownJTs.map(pullUp))
//    assert(pushedDownJTs == pushedDownJTs.map(pushDown))

    //turn the BitSets into scala sets
    val resultJT: Seq[Tree[(Set[Int], Seq[A])]] = pushedDownJTs.map(jt => jt.map(treeNode => (bs2Iterator(treeNode._1).toSet, treeNode._2)))

    println("jt test: " + isJunctionTree(_cliques,resultJT))
    (resultJT,tw)
  }

  /** Checks the junctiontreedness of the arguments. You need a working equals on the `A`s. */
  def isJunctionTree[A](cliques: IndexedSeq[(Set[Int],A)], trees: Seq[Tree[(Set[Int],Seq[A])]]): Boolean = {
    //each clique has to appear exactly once
    val cliqueCountsFromTrees: Map[A, Int] = trees.map(_.flatten.map(_._2).flatten).flatten.groupBy(identity).mapValues(_.size)
    val cliqueCountsFromReference: Map[A, Int] = cliques.map(_._2).groupBy(identity).mapValues(_.size)
    val allCliquesPresent: Boolean = cliqueCountsFromTrees == cliqueCountsFromReference
    assert(allCliquesPresent, "the counts differ")

    @tailrec
    def computeNeighbourhoods(cliques: List[(Set[Int],A)], acc: Seq[Set[A]] = Nil): Seq[Set[A]] = cliques match {
      case Nil => acc
      case (hv,ha) :: tail => computeNeighbourhoods(
        tail,
        tail.collect{case (tv,ta) if (!hv.intersect(tv).isEmpty) => Set(ha,ta)} ++ acc)
    }

//    val neighbourhoods = computeNeighbourhoods(cliques.toList)
//    println("going to check on %d neighbourhoods".format(neighbourhoods.size))
//    //this must be fucking slow
//    val allDependenciesPresent = neighbourhoods.forall{neigh =>
//      val isRepresented = trees.exists(tree => tree.flatten.exists(node => neigh.forall(node._2.contains)))
//      assert(isRepresented, {
//        printJTs(trees)
//        "two adjacent nodes exist, that are not together in some junction: " + neigh
//      })
//      isRepresented
//    }

    //the variables in active may still appear, the variables in spent may not appear again
    def checkTree(t: Tree[Set[Int]], active: Set[Int] = Set(), spent: Set[Int] = Set()): Boolean = {
      val label = t.rootLabel
      val reappearers: Set[Int] = (spent -- active).intersect(label)
      if(!reappearers.isEmpty){
        println("hit variable(s) again: " + reappearers)
        false
      } else {
        val newSpent = spent ++ label
        t.subForest.forall(subTree => checkTree(subTree,label,newSpent))
      }
    }
    //check the running intersection property: the junctions a certain variable appears in form  a subtree
    val runningIntersection = trees.map(_.map(_._1)).forall(checkTree(_))
    //(i.e. they are connected)
    allCliquesPresent && runningIntersection
  }

  def printJTs[A](trees: Seq[Tree[A]]): Unit = println(trees.map(_.map(_.toString).drawTree).mkString("\n---\n"))

  def minDegreeOrdering(cliques: Seq[Set[Int]]): List[Int] = minDegreeOrderingAndWidth(cliques.toIndexedSeq)._1//minDegreeOrderingAndWidthSlow(cliques)._1

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