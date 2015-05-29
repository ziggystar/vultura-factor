package vultura.factor.inference

import vultura.factor._
import vultura.util.TreeWidth._
import vultura.util._
import vultura.util.graph.Tree
import vultura.util.graph.Tree._

import scala.collection.mutable
import scala.util.Random

/** Ordinary Shanoy-Shafer (1990) junction tree algorithm. */
class JunctionTree(val problem: Problem, val variableOrderer: VariableOrderer = MinDegreeOrderer) extends MargParI with JointMargI {

  val variableOrder: VariableOrder = variableOrderer(problem)

  val (calibratedTrees: Seq[Tree[Factor]], mlogZ) = {
    val calibratedTreesWithZ = uncalibratedTrees.map(JunctionTree.calibrate(_,problem.ring, problem.domains))
    (calibratedTreesWithZ.map(_._1),problem.ring.prodA(calibratedTreesWithZ.map(_._2)(collection.breakOut)))
  }

  val Z = mlogZ

  lazy val calibratedCliques: Map[Set[Var],Factor] =
    calibratedTrees.flatMap(_.flatten).groupBy(_.variables.toSet).map{case (k,v) => k -> v.head}
  lazy val ssetCliques = new SSet(calibratedCliques.keySet)

  def uncalibratedTrees: Seq[Tree[Factor]] = {
    //1. create format for jt-creation
    //2. multiply all factors of each clique into one
    compactJTrees(initialTrees)
        .map(_.map {
        case (vars, factors) => Factor.multiplyRetain(problem.ring)(problem.domains)(factors.toIndexedSeq,vars.toArray.sorted)
      })
    }

  def initialTrees: Seq[Tree[(Set[Int], Seq[Factor])]] =
    junctionTreesFromOrder(problem.factors.map(f => f.variables.toSet -> f), variableOrder.order.toList)

  private val marginalCache = new mutable.HashMap[Int, Factor]()

  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def variableBelief(vi: Int): Factor = marginalCache.getOrElseUpdate(vi, cliqueBelief(Array(vi)))

  /** Throws if no clique contains `vars`.
    * @return Normalized belief over given variables in encoding specified by problem ring. */
  def cliqueBelief(vars: Array[Var]): Factor = {
    val containingClique: Factor = calibratedCliques(ssetCliques.superSetsOf(vars.toSet).minBy(_.size))
    Factor.multiplyRetain(problem.ring)(problem.domains)(
      Seq(containingClique),
      vars).normalize(problem.ring)
  }

  def sample(r: Random): Map[Var,Val] = calibratedTrees.map{ tree =>
    tree.mapDown(Map[Var,Val]()){ case (cond,factor) =>
      val conditionedFactor: Factor = factor.condition(cond, problem.domains)
      (conditionedFactor.variables zip conditionedFactor.sample(r, problem.domains, problem.ring))(collection.breakOut)
    }.flatten.reduce(_ ++ _)
  }.foldLeft(Map[Var,Val]())(_ ++ _)

  def graphviz: String = {
    def nodeName(cliqueFactor: Factor): String = "n" + cliqueFactor.variables.mkString("_")

    val calibratedTrees: Seq[Tree[(Factor, Set[Int], Factor, Factor)]] =
      uncalibratedTrees.map(JunctionTree.calibrateTree(_,problem.ring, problem.domains))

    val nodes = calibratedTrees.flatMap(_.flatten.map{case (cliqueFactor,_,_,_) =>
      f"""${nodeName(cliqueFactor)} [label="${cliqueFactor.toBriefString}"]"""
    })

    val edges: Seq[((Factor, Factor), Factor)] =
      calibratedTrees.flatMap(_.cobind[Seq[((Factor,Factor),Factor)]]{ case Node((cliqueFactor,_,_,_),children) =>
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
  def calibrate(tree: Tree[Factor], ring: Ring[Double], domains: Array[Int]): (Tree[Factor], Double) = {
    val calTree: Tree[(Factor,Set[Int],Factor,Factor)] = calibrateTree(tree,ring, domains)
    val compressedTree: Tree[Factor] = calTree.cobind{
      case Node((factor,_,_,downMessage),children) =>
        Factor.multiply(ring)(domains)(
          (children.map(_.rootLabel._3) :+ downMessage :+ factor)(collection.breakOut)
        )
    }
    (compressedTree, calTree.rootLabel._3.values(0))
  }

  /** @return Tuple entries are: (clique factor, sepset with parent, message to parent, message from parent). */
  def calibrateTree(tree: Tree[Factor],
                    ring: Ring[Double],
                    domains: Array[Int]): Tree[(Factor, Set[Int], Factor, Factor)] = {
    val withSepsets: Tree[(Factor, Set[Int])] =
      tree.scand(Set[Int]()){ case (parentSet, subtree) =>
        val newLabel: (Factor, Set[Int]) = (subtree.rootLabel, parentSet)
        val childSepSets: Stream[Set[Int]] =
          subtree.subForest.map(_.rootLabel.variables.toSet.intersect(subtree.rootLabel.variables.toSet))
        (newLabel, childSepSets)
      }
    val upwardCalibrated: Tree[(Factor,Set[Int],Factor)] =
      withSepsets.scanr[(Factor,Set[Int],Factor)]{ case ((factor,sepset),children) =>
        val upwardMessage = Factor.multiplyRetain(ring)(domains)(
          children.map(_.rootLabel._3).toIndexedSeq :+ factor,
          sepset.toArray.sorted
        )
        (factor,sepset,upwardMessage)
      }
    upwardCalibrated.scand(Factor(Array(),Array(ring.one))){
      case (downMessage,Node((factor,sepset,upmessage),children)) =>
        val newLabel = (factor,sepset,upmessage,downMessage)
        val downMessages = mapOthers2(children.map(_.rootLabel)){case (childLabel,otherChildLabels) =>
          Factor.multiplyRetain(ring)(domains)(
            otherChildLabels.map(_._3).toIndexedSeq :+ factor :+ downMessage,
            childLabel._2.toArray.sorted
          )
        }
        (newLabel,downMessages)
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
