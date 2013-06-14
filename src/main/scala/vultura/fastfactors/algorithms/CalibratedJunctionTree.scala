package vultura.fastfactors.algorithms

import vultura.fastfactors.{LogD, FastFactor, Problem}
import vultura.util.TreeWidth._
import scalaz._
import Scalaz._
import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/14/13
 */

class CalibratedJunctionTree(val problem: Problem) extends InfAlg {
  def getProblem: Problem = problem
  val (trees: Seq[Tree[FastFactor]], myLogZ) = {
    val pretrees: Seq[Tree[(Set[Int], Seq[FastFactor])]] = compactJTrees(minDegreeJTs(problem.factors.map(f => f.variables.toSet -> f)))
    val calibratedTreesWithZ = pretrees
      .map(_.map{case (vars, factors) => FastFactor.multiply(problem.ring)(problem.domains)(factors.toIndexedSeq)})
      .map(calibrate)
    (calibratedTreesWithZ.map(_._1),problem.ring.prodA(calibratedTreesWithZ.map(_._2)(collection.breakOut)))
  }

  /** Discards the tree structure and returns calibrated cliques and the partition function for the tree. */
  def calibrate(tree: Tree[FastFactor]): (Tree[FastFactor], Double) = {
    import CalibratedJunctionTree._
    val withSepsets: Tree[(FastFactor, Set[Int])] =
      scand(tree)(Set[Int]()){ case (parentSet, subtree) =>
        val newLabel: (FastFactor, Set[Int]) = (subtree.rootLabel, parentSet)
        val childSepSets: Stream[Set[Int]] =
          subtree.subForest.map(_.rootLabel.variables.toSet.intersect(subtree.rootLabel.variables.toSet))
        (newLabel, childSepSets)
      }
    val upwardCalibrated: Tree[(FastFactor,Set[Int],FastFactor)] =
      withSepsets.scanr[(FastFactor,Set[Int],FastFactor)]{ case ((factor,sepset),children) =>
        val upwardMessage = FastFactor.multiplyRetain(problem.ring)(problem.domains)(
          children.map(_.rootLabel._3).toIndexedSeq :+ factor,
          sepset.toArray.sorted
        )
        (factor,sepset,upwardMessage)
      }
    val upDownCalibrated: Tree[(FastFactor,Set[Int],FastFactor,FastFactor)] =
      scand(upwardCalibrated)(FastFactor(Array(),Array(problem.ring.one))){
        case (downMessage,Node((factor,sepset,upmessage),children)) => {
          val newLabel = (factor,sepset,upmessage,downMessage)
          val downMessages = mapOthers2(children.map(_.rootLabel)){case (childLabel,otherChildLabels) =>
            FastFactor.multiplyRetain(problem.ring)(problem.domains)(
              otherChildLabels.map(_._3).toIndexedSeq :+ factor :+ downMessage,
              childLabel._2.toArray.sorted
            )
          }
          (newLabel,downMessages)
        }
      }
    val calibratedTree: Tree[FastFactor] = upDownCalibrated.cobind{
      case Node((factor,_,downMessage,_),children) =>
        FastFactor.multiply(problem.ring)(problem.domains)(
          (children.map(_.rootLabel._3) :+ downMessage :+ factor)(collection.breakOut)
        )
    }
    (calibratedTree, problem.ring.sumA(calibratedTree.rootLabel.values))
  }

  /** @return Natural logarithm of partition function. */
  def logZ: Double = if(problem.ring == LogD) myLogZ else math.log(problem.ring.decode(Array(myLogZ))(0))

  /** @return Partition function in encoding specified by `ring`. */
  def Z: Double = myLogZ

  private val marginalCache = new mutable.HashMap[Int, FastFactor]()
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  def variableBelief(vi: Int): FastFactor = marginalCache.getOrElseUpdate(vi,
    trees.flatMap(_.flatten)
      .find(_.variables.contains(vi))
      .map(f => FastFactor.multiplyRetain(problem.ring)(problem.domains)(Seq(f),Array(vi)).normalize(problem.ring))
      .get
  )

  /** @return marginal distribution of variable in log encoding. */
  def logVariableBelief(vi: Int): FastFactor =
    if(problem.ring == LogD) variableBelief(vi) else LogD.encode(problem.ring.decode(variableBelief(vi)))
}

object CalibratedJunctionTree{
  /** downward propagation in trees. */
  def scand[A,B,C](tree: Tree[A])(init: B)(f: (B,Tree[A]) => (C,Seq[B])): Tree[C] = {
    val (newVal, childPropagations) = f(init,tree)
    node(newVal,tree.subForest.zip(childPropagations).map{case (child,childProp) => scand(child)(childProp)(f)})
  }

  /** Return xs without the element at position idx. */
  def others[B, A](xs: scala.Seq[A], idx: Int): Seq[A] =  xs.take(idx) ++ xs.drop(idx + 1)

  /** Also gives the current element as argument to f. */
  def mapOthers2[A,B](xs: Seq[A])(f: (A,Seq[A]) => B): Seq[B] = {
    for(
      (x,idx) <- xs.zipWithIndex;
      otherx = others(xs, idx)
    ) yield f(x,otherx)
  }
}
