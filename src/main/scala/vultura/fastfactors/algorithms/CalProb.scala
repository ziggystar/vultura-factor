package vultura.fastfactors.algorithms

import scala.collection.mutable
import vultura.util._
import scala.reflect.ClassTag

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
trait CalProb {
  def edges: Set[CEdge]
  def nodes: Set[CNode] = edges.flatMap(e => e.input :+ e.output)
}

trait CEdge {
  type TIn <: Any
  type TOut <: Any
  def input: IndexedSeq[CNode{type R <: TIn}]
  def output: CNode{type R = TOut}
  def compute: IndexedSeq[TIn] => TOut
}

trait CNode {
  /** The mutable representation type. Usually unboxed Arrays. */
  type R <: Any
  def create: R
  def diff(r1: R, r2: R): Double
  def printValue(v: R): String = v.toString
}

class Calibrator(problem: CalProb, tol: Double = 1e-9, maxSteps: Int = 1000){

  val nodeList: IndexedSeq[CNode] = problem.nodes.toIndexedSeq
  val edgeList: IndexedSeq[CEdge] = problem.edges.toIndexedSeq

  //when a node changes its state, this tells us which edges need to be recomputed
  val dependentEdges: Map[CNode, IndexedSeq[CEdge]] = {
    val broken: IndexedSeq[(CNode, CEdge)] = (for(e <- problem.edges; in <- e.input) yield in -> e)(collection.breakOut)
    broken.groupByMap(by = _._1, f = _._2)
  }

  val nodeIndex: Map[CNode, Int] = nodeList.zipWithIndex.toMap
  val edgeIndex: Map[CEdge, Int] = edgeList.zipWithIndex.toMap

  private val state: mutable.Buffer[Any] = nodeList.map(_.create).toBuffer

  //when was an edge calibrated the last time?
  private val lastCalibrated: Array[Int] = Array.fill(edgeList.size)(-1)

  private val dirtyEdges: mutable.Queue[(CEdge,Int)] = mutable.Queue((for(e <- edgeList) yield e -> 0): _*)

  private var steps: Int = 0

  private def nodeState(n: CNode): n.R = state(nodeIndex(n)).asInstanceOf[n.R]

  calibrate()

  /** Assumes that `e` is already dequeued, and steps is incremented. */
  private def updateEdge(e: CEdge): Boolean = {
    val input: IndexedSeq[e.TIn] = e.input.map(n => nodeState(n).asInstanceOf[e.TIn])(collection.breakOut)
    //recalculate
    val newVal: e.TOut = e.compute(input)
    val output = e.output
    val diff = output.diff(newVal, nodeState(output))
    if(diff > tol) {
      //save new state
      state(nodeIndex(output)) = newVal
      //awake dependent edges
      dirtyEdges.enqueue(dependentEdges(output).map(e => e -> steps):_*)
      true
    }
    else false
  }

  def calibrate(): Unit = {
    while(!dirtyEdges.isEmpty || steps >= maxSteps){
      val (e,lastUpdate) = dirtyEdges.dequeue()
      if(lastUpdate > lastCalibrated(edgeIndex(e))) {
        //recompute
        if(updateEdge(e))
          steps = steps + 1
      }
    }
  }

  def isCalibrated = dirtyEdges.isEmpty

  def valuesString: String = nodeList.map(n => s"$n -> ${n.printValue(nodeState(n))}").mkString("\n")
}
