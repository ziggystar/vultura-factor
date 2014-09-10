package vultura.factor.inference.calibration

import vultura.util._
import vultura.util.graph.DotGraph

import scala.collection.mutable

/** Calibrator that supports mutable message updates.
  * Edges are updated in a round-robin way. Only those updates are performed that
  * result in a significant change according to `differ`.
  * @param maxSteps Number of edge updates to perform initially.
  */
class MutableFIFOCalibrator[E <: MEdge](val problem: Iterable[E])(
  val convergenceTest: ConvergenceTest[E] = ConvergenceTest.MaxDiff(),
  val maxSteps: Long = 1000000,
  initialize: EdgeValues[E]) extends Calibrated[E] {

  class EdgeData[ET <: E](val e: ET) {
    val inputSpace = new mutable.ArraySeq[e.InEdge#TOut](e.inputs.size)
    val computation: (IndexedSeq[e.InEdge#TOut], e.TOut) => Unit = e.mCompute()

    override def toString: String = s"ED: $e"
  }

  type Out = E#TOut
  val edgeIndex: SIIndex[E] = new SIIndex(problem.toSet)
  //edge index
  type EI = edgeIndex.Idx
  val numEdges: Int = edgeIndex.size

  final def edges: IndexedSeq[E] = edgeIndex.elements

  //immutable
  private val edgeData: Array[EdgeData[E]] = edges.map(new EdgeData(_))(collection.breakOut)

  //the predecessors of an edge; immutable
  private val predecessors: Array[Array[EI]] = edgeIndex.createArrayLookup(_.inputs.map(_.asInstanceOf[E]))

  //the successors of an edge
  val successors: IndexedSeq[Array[EI]] = {
    //if edges is closed, the following cast succeeds (because E is a valid super type)
    val fromTo: IndexedSeq[(EI, EI)] = for (toI <- 0 until numEdges; fromI <- predecessors(toI)) yield (fromI, toI)
    ((0 until numEdges) map fromTo.groupByMap(_._1, _._2).withDefaultValue(IndexedSeq())).map(_.toArray)
  }

  /** Access initializer only through method to avoid keeping a reference to it. */
  private def computeInitializedState(ev: EdgeValues[E]): mutable.Buffer[Out] = edges.map{ e =>
    assert(ev.hasEdge(e))
    e.copy(ev.edgeValue(e))
  }.toBuffer

  //the current values of the edges
  private val state: mutable.Buffer[Out] = computeInitializedState(initialize)

  //object pool for calculating new values
  private val pool: mutable.Buffer[Out] = edges.map(_.create).toBuffer

  //when was an edge calibrated the last time?
  private val lastCalibrated: Array[Long] = Array.fill(numEdges)(-1)

  private val dirtyEdges: MutableArrayQueue[(Int, Long)] = MutableArrayQueue(for (e <- 0 until numEdges) yield e -> 0L)

  private var steps: Long = 0

  calibrate()

  /** Assumes that `e` is already dequeued, and steps is incremented. */
  private def updateEdge(ei: Int): Boolean = {
    val e: E = edgeIndex.backward(ei)
    val eData: EdgeData[e.type] = edgeData(ei).asInstanceOf[EdgeData[e.type]]

    val input: mutable.ArraySeq[e.InEdge#TOut] = {
      val preds: Array[EI] = predecessors(ei)
      val b = eData.inputSpace
      var i = 0
      while (i < b.length) {
        b(i) = state(preds(i)).asInstanceOf[e.InEdge#TOut]
        i += 1
      }
      b
    }

    val newVal = pool(ei).asInstanceOf[e.TOut]
    val oldVal = state(ei).asInstanceOf[e.TOut]
    //recalculate
    eData.computation(input, newVal)

    if (!convergenceTest.isConverged(e)(oldVal,newVal)) {
      //save new state
      pool(ei) = oldVal
      state(ei) = newVal
      //awake dependent edges
      successors(ei).foreach { e =>
        dirtyEdges.enqueue((e, steps))
      }
      lastCalibrated(ei) = steps
      true
    }
    else false
  }

  def calibrate(): Unit = {
    while (dirtyEdges.nonEmpty && steps < maxSteps) {
      val (ei, lastUpdate) = dirtyEdges.dequeue()
      if (lastUpdate > lastCalibrated(ei)) {
        //side-effect
        val wasChanged: Boolean = updateEdge(ei)
        if (wasChanged)
          steps = steps + 1
      }
    }
  }

  def lastUpdateOf(e: E): Long = lastCalibrated(edgeIndex(e))

  def iteration: Long = steps

  def edgeValue(n: E): n.TOut = state(edgeIndex(n)).asInstanceOf[n.TOut]

  def hasEdge(e: E): Boolean = edgeIndex.contains(e)

  def isConverged = dirtyEdges.isEmpty

  def result: EdgeValues[E] = new EdgeValues[E] {
    private val index: SIIndex[E] = edgeIndex
    private val values: mutable.Buffer[Out] = state

    override def hasEdge(e: E): Boolean = index.contains(e)

    override def edgeValue(e: E): e.type#TOut = values(index(e)).asInstanceOf[e.TOut]
  }

  def toDot: DotGraph[Int] = DotGraph(
    edges = for{
      (preds,succ) <- predecessors.zipWithIndex
      pred <- preds
    } yield pred -> succ,
    graphName = "MutableFIFOCalibrator"
  ).nodeLabeled { ei =>
    val edge: E = edges(ei)
    s"$edge\\n ${edge.prettyPrint(edgeValue(edge))}"
  }
}
