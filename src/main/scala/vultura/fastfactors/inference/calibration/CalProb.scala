package vultura.fastfactors.inference.calibration

import scala.collection.mutable
import vultura.util._
import vultura.util.graph.DotGraph

/** A CEdge describes the functional dependency of a target node on a set of other nodes. Note that a CEdge
  * describes both a directed hyper-edge *and* the target node.
  */
trait CEdge {
  /** Representation type of the node itself. */
  type TOut <: Any
  /** Representation type of the independent nodes/edges. */
  type ETIn <: CEdge
  type TIn = ETIn#TOut
  /** Create a (mutable???) representation of the initial value of this node. */
  def create: TOut
  /** @return the change between two values of this node. Zero means no change, lower means less change. */
  def diff(r1: TOut, r2: TOut): Double
  /** The nodes this edge depends on. This must remain lazy. */
  def input: IndexedSeq[ETIn]
  /** Compute the value of this node given the values of the independent nodes. */
  def compute: IndexedSeq[TIn] => TOut

  def tabEntry: String = this.toString
  /** @return human readable representation of a value. */
  def printValue(v: TOut): String = v.toString
}

object CEdge {
  /** @return The transitive closure, following the edges backwards. */
  def expand(edges: Set[CEdge], closed: Set[CEdge] = Set()): Set[CEdge] = if(edges.isEmpty) closed else {
    val newClosed = edges ++ closed
    val preds: Set[CEdge] = (for {
      e <- edges
      in <- e.input if !newClosed(in)
    } yield in)(collection.breakOut)
    expand(preds, newClosed)
  }
}

class Calibrator(edges: Set[CEdge], tol: Double = 1e-9, maxSteps: Int = 1000){
  val edgeList: IndexedSeq[CEdge] = edges.toIndexedSeq
  val edgeIndex: collection.Map[CEdge, Int] = mutable.HashMap(edgeList.zipWithIndex:_*)

  //when a node changes its state, this tells us which edges need to be recomputed
  val dependentEdges: Map[CEdge, IndexedSeq[CEdge]] = {
    val broken: IndexedSeq[(CEdge, CEdge)] = for(e <- edgeList; in <- e.input) yield in -> e
    broken.groupByMap(_._1,_._2).withDefaultValue(IndexedSeq())
  }

  private val state: mutable.Buffer[Any] = edgeList.map(_.create).toBuffer

  //when was an edge calibrated the last time?
  private val lastCalibrated: Array[Int] = Array.fill(edgeList.size)(-1)

  private val dirtyEdges: mutable.Queue[(CEdge,Int)] = mutable.Queue((for(e <- edgeList) yield e -> 0): _*)

  private var steps: Int = 0

  def iteration: Int = steps

  def nodeState(n: CEdge): n.TOut = state(edgeIndex(n)).asInstanceOf[n.TOut]

  val inputMemo: Map[CEdge,IndexedSeq[CEdge]] = edges.map(e => e -> e.input)(collection.breakOut)

  calibrate()

  /** Assumes that `e` is already dequeued, and steps is incremented. */
  private def updateEdge(e: CEdge): Boolean = {
    val input: IndexedSeq[e.TIn] = inputMemo(e).map(n => nodeState(n).asInstanceOf[e.TIn])(collection.breakOut)
    //recalculate
    val newVal: e.TOut = e.compute(input)
    val diff = e.diff(newVal, nodeState(e))
    if(diff > tol) {
      //save new state
      state(edgeIndex(e)) = newVal
      //awake dependent edges
      dirtyEdges.enqueue(dependentEdges(e).map(e => e -> steps):_*)
      lastCalibrated(edgeIndex(e)) = steps
      true
    }
    else false
  }

  def calibrate(): Unit = {
    while(!dirtyEdges.isEmpty && steps < maxSteps){
      val (e,lastUpdate) = dirtyEdges.dequeue()
      if(lastUpdate > lastCalibrated(edgeIndex(e))) {
        //recompute
        if(updateEdge(e)) //side-effect
          steps = steps + 1
      }
    }
  }

  def isCalibrated = dirtyEdges.isEmpty

  def valuesString: String = edgeList.map(n => s"$n -> ${n.printValue(nodeState(n))}").mkString("\n")

  def dot: DotGraph[CEdge] = DotGraph[CEdge](edges=edgeList.flatMap(n => n.input.map(_ -> n)))

  def report: String = edgeList
    .map(e => e.tabEntry + "\n\t" + e.printValue(nodeState(e).asInstanceOf[e.TOut]))
    .sorted.mkString("\n")
}