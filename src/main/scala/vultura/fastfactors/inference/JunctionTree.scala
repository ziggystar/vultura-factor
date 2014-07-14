package vultura.fastfactors.inference

import vultura.fastfactors._
import vultura.util.ScalazUtils._
import vultura.util.TreeWidth._
import vultura.util._

import scala.collection.mutable
import scala.util.Random
import scalaz.Tree._
import scalaz._

/** Ordinary Shanoy-Shafer (1990) junction tree algorithm. */
class JunctionTree(val problem: Problem, val variableOrderer: VariableOrderer = MinDegreeOrderer) extends MargParI with JointMargI {

  val variableOrder: VariableOrder = variableOrderer(problem)

  val (calibratedTrees: Seq[Tree[FastFactor]], myLogZ) = {
    val calibratedTreesWithZ = uncalibratedTrees.map(JunctionTree.calibrate(_,problem.ring, problem.domains))
    (calibratedTreesWithZ.map(_._1),problem.ring.prodA(calibratedTreesWithZ.map(_._2)(collection.breakOut)))
  }

  lazy val calibratedCliques: Map[Set[Var],FastFactor] =
    calibratedTrees.map(_.flatten).flatten.groupBy(_.variables.toSet).map{case (k,v) => k -> v.head}
  lazy val ssetCliques = new SSet(calibratedCliques.keySet)

  def uncalibratedTrees: Seq[Tree[FastFactor]] = {
    //1. create format for jt-creation
    //2. multiply all factors of each clique into one
    compactJTrees(initialTrees)
        .map(_.map {
        case (vars, factors) => FastFactor.multiplyRetain(problem.ring)(problem.domains)(factors.toIndexedSeq,vars.toArray.sorted)
      })
    }

  def initialTrees: Seq[Tree[(Set[Int], Seq[FastFactor])]] =
    junctionTreesFromOrder(problem.factors.map(f => f.variables.toSet -> f), variableOrder.order.toList)

  /** @return Partition function in encoding specified by `ring`. */
  def Z: Double = problem.ring.decode(Array(myLogZ))(0)
  /** @return Natural logarithm of partition function. */
  override def logZ: Double = if(problem.ring == LogD) myLogZ else math.log(problem.ring.decode(Array(myLogZ))(0))

  private val marginalCache = new mutable.HashMap[Int, FastFactor]()

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def variableBelief(vi: Int): FastFactor = marginalCache.getOrElseUpdate(vi, cliqueBelief(Array(vi)))

  /** Throws if no clique contains `vars`.
    * @return Normalized belief over given variables in encoding specified by problem ring. */
  def cliqueBelief(vars: Array[Var]): FastFactor = {
    val containingClique: FastFactor = calibratedCliques(ssetCliques.superSetsOf(vars.toSet).minBy(_.size))
    FastFactor.multiplyRetain(problem.ring)(problem.domains)(
      Seq(containingClique),
      vars).normalize(problem.ring)
  }

  def sample(r: Random): Map[Var,Val] = calibratedTrees.map{ tree =>
    tree.mapDown(Map[Var,Val]()){ case (cond,factor) =>
      val conditionedFactor: FastFactor = factor.condition(cond, problem.domains)
      (conditionedFactor.variables zip conditionedFactor.sample(r, problem.domains, problem.ring))(collection.breakOut)
    }.flatten.reduce(_ ++ _)
  }.foldLeft(Map[Var,Val]())(_ ++ _)

  def graphviz: String = {
    def nodeName(cliqueFactor: FastFactor): String = "n" + cliqueFactor.variables.mkString("_")

    val calibratedTrees: Seq[Tree[(FastFactor, Set[Int], FastFactor, FastFactor)]] =
      uncalibratedTrees.map(JunctionTree.calibrateTree(_,problem.ring, problem.domains))

    val nodes = calibratedTrees.flatMap(_.flatten.map{case (cliqueFactor,_,_,_) =>
      f"""${nodeName(cliqueFactor)} [label="${cliqueFactor.toBriefString}"]"""
    })

    val edges: Seq[((FastFactor, FastFactor), FastFactor)] =
      calibratedTrees.flatMap(_.cobind[Seq[((FastFactor,FastFactor),FastFactor)]]{ case Node((cliqueFactor,_,_,_),children) =>
        children.map{case Node((childFactor,_,up,down),_) => Seq((cliqueFactor -> childFactor,down),(childFactor -> cliqueFactor,up))
        }.flatten
      }.flatten).flatten
    val edgeStrings = edges.map{case ((from,to),msg) => f"""${nodeName(from)} -> ${nodeName(to)} [label="${msg.toBriefString}"]"""}

    "digraph CalibratedJunctionTree {\n" +
      nodes.mkString("\n") + "\n" +
      "\n" +
      edgeStrings.mkString("\n") + "\n" +
      "}"
  }
}

object JunctionTree{
  def logZ(p: Problem): Double = new JunctionTree(p).logZ
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
      tree.scand(Set[Int]()){ case (parentSet, subtree) =>
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
    upwardCalibrated.scand(FastFactor(Array(),Array(ring.one))){
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
