package vultura.fastfactors.algorithms

import vultura.fastfactors.{RingZ, LogD, FastFactor, Problem}
import vultura.util.TreeWidth._
import scalaz._
import Scalaz._
import scalaz.Tree._
import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 6/14/13
 */

class CalibratedJunctionTree(val problem: Problem) extends InfAlg {
  def getProblem: Problem = problem
  val (calibratedTrees: Seq[Tree[FastFactor]], myLogZ) = {
    val calibratedTreesWithZ = uncalibratedTrees.map(CalibratedJunctionTree.calibrate(_,problem.ring, problem.domains))
    (calibratedTreesWithZ.map(_._1),problem.ring.prodA(calibratedTreesWithZ.map(_._2)(collection.breakOut)))
  }

  //TODO [design] doesn't make much sense?
  def iteration: Int = 1

  def uncalibratedTrees: Seq[Tree[FastFactor]] = {
    //1. create format for jt-creation
    //2. multiply all factors of each clique into one
    compactJTrees(minDegreeJTs(problem.factors.map(f => f.variables.toSet -> f)))
        .map(_.map {
        case (vars, factors) => FastFactor.multiplyRetain(problem.ring)(problem.domains)(factors.toIndexedSeq,vars.toArray.sorted)
      })
    }

  /** @return Partition function in encoding specified by `ring`. */
  def Z: Double = myLogZ

  private val marginalCache = new mutable.HashMap[Int, FastFactor]()
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  def variableBelief(vi: Int): FastFactor = marginalCache.getOrElseUpdate(vi,
    calibratedTrees.flatMap(_.flatten)
      .find(_.variables.contains(vi))
      .map(f => FastFactor.multiplyRetain(problem.ring)(problem.domains)(Seq(f),Array(vi)).normalize(problem.ring))
      .get
  )

  def graphviz: String = {
    def nodeName(cliqueFactor: FastFactor): String = "n" + cliqueFactor.variables.mkString("_")

    val calibratedTrees: Seq[Tree[(FastFactor, Set[Int], FastFactor, FastFactor)]] =
      uncalibratedTrees.map(CalibratedJunctionTree.calibrateTree(_,problem.ring, problem.domains))

    val nodes = calibratedTrees.flatMap(_.flatten.map{case (cliqueFactor,_,_,_) =>
      f"""${nodeName(cliqueFactor)} [label="${cliqueFactor.toStringShort}"]"""
    })

    val edges: Seq[((FastFactor, FastFactor), FastFactor)] =
      calibratedTrees.flatMap(_.cobind[Seq[((FastFactor,FastFactor),FastFactor)]]{ case Node((cliqueFactor,_,_,_),children) =>
        children.map{case Node((childFactor,_,up,down),_) => Seq((cliqueFactor -> childFactor,down),(childFactor -> cliqueFactor,up))
        }.flatten
      }.flatten).flatten
    val edgeStrings = edges.map{case ((from,to),msg) => f"""${nodeName(from)} -> ${nodeName(to)} [label="${msg.toStringShort}"]"""}

    "digraph CalibratedJunctionTree {\n" +
      nodes.mkString("\n") + "\n" +
      "\n" +
      edgeStrings.mkString("\n") + "\n" +
      "}"
  }
}

object CalibratedJunctionTree{
  /** Discards the tree structure and returns calibrated cliques and the partition function for the tree. */
  def calibrate(tree: Tree[FastFactor], ring: RingZ[Double], domains: Array[Int]): (Tree[FastFactor], Double) = {
    val calTree: Tree[(FastFactor,Set[Int],FastFactor,FastFactor)] = calibrateTree(tree,ring, domains)
    val compressedTree: Tree[FastFactor] = calTree.cobind{
      case Node((factor,_,_,downMessage),children) =>
        FastFactor.multiply(ring)(domains)(
          (children.map(_.rootLabel._3) :+ downMessage :+ factor)(collection.breakOut)
        )
    }
    (compressedTree, calTree.rootLabel._3.values(0))
  }

  /** @return Tuple entries are: (clique factor, sepset with parent, message to parent, message from parent). */
  def calibrateTree(tree: Tree[FastFactor],
                    ring: RingZ[Double],
                    domains: Array[Int]): Tree[(FastFactor, Set[Int], FastFactor, FastFactor)] = {
    val withSepsets: Tree[(FastFactor, Set[Int])] =
      scand(tree)(Set[Int]()){ case (parentSet, subtree) =>
        val newLabel: (FastFactor, Set[Int]) = (subtree.rootLabel, parentSet)
        val childSepSets: Stream[Set[Int]] =
          subtree.subForest.map(_.rootLabel.variables.toSet.intersect(subtree.rootLabel.variables.toSet))
        (newLabel, childSepSets)
      }
    val upwardCalibrated: Tree[(FastFactor,Set[Int],FastFactor)] =
      withSepsets.scanr[(FastFactor,Set[Int],FastFactor)]{ case ((factor,sepset),children) =>
        val upwardMessage = FastFactor.multiplyRetain(ring)(domains)(
          children.map(_.rootLabel._3).toIndexedSeq :+ factor,
          sepset.toArray.sorted
        )
        (factor,sepset,upwardMessage)
      }
    scand(upwardCalibrated)(FastFactor(Array(),Array(ring.one))){
      case (downMessage,Node((factor,sepset,upmessage),children)) => {
        val newLabel = (factor,sepset,upmessage,downMessage)
        val downMessages = mapOthers2(children.map(_.rootLabel)){case (childLabel,otherChildLabels) =>
          FastFactor.multiplyRetain(ring)(domains)(
            otherChildLabels.map(_._3).toIndexedSeq :+ factor :+ downMessage,
            childLabel._2.toArray.sorted
          )
        }
        (newLabel,downMessages)
      }
    }
  }

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
