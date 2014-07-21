package vultura.factor.inference.calibration

import vultura.util._
import collection.mutable
import scala.collection.mutable

/**
 * This calibrator can be used for immutable edge types.
 *
 * The calibrator itself contains mutable state, and thus is not thread-safe.
 *
 * @param maxSteps Run this many edge updates after construction.
 *
 * @see Edge, EdgeValues
 */
class FIFOCalibrator[E <: Edge](val problem: Iterable[E])(
  val convergenceTest: ConvergenceTest[E] = ConvergenceTest.MaxDiff(),
  val maxSteps: Long = 1000000,
  initialize: EdgeValues[E]) extends Calibrated[E]{
  val edges: SIIndex[E] = new SIIndex(problem.toSet)
  def edgeIndex(e: E): Int = edges.forward(e)

  //when a node changes its state, this tells us which edges need to be recomputed
  val dependentEdges: Map[E, IndexedSeq[E]] = {
    val broken: IndexedSeq[(E, E)] = for(e <- edges.elements; in <- e.inputs) yield in.asInstanceOf[E] -> e
    broken.groupByMap(_._1,_._2).withDefaultValue(IndexedSeq())
  }

  private val state: mutable.Buffer[Any] = edges.elements.map(e => initialize.edgeValue(e)).toBuffer

  //when was an edge calibrated the last time?
  private val lastCalibrated: Array[Long] = Array.fill(edges.elements.size)(-1)

  private val dirtyEdges: mutable.Queue[(E,Long)] = mutable.Queue((for(e <- edges.elements) yield e -> 0L): _*)

  private var steps: Long = 0

  def iteration: Long = steps

  def edgeValue(n: E): n.TOut = state(edgeIndex(n)).asInstanceOf[n.TOut]

  val inputMemo: Map[E,IndexedSeq[E]] = edges.elements.map(e => e -> e.inputs.map(_.asInstanceOf[E]))(collection.breakOut)

  calibrate()

  /** Assumes that `e` is already dequeued, and steps is incremented. */
  private def updateEdge(e: E): Boolean = {
    val input: IndexedSeq[e.InEdge#TOut] = inputMemo(e).map(n => edgeValue(n).asInstanceOf[e.InEdge#TOut])(collection.breakOut)
    //recalculate
    val newVal: e.TOut = e.compute(input)
    if(convergenceTest.isConverged(e)(edgeValue(e),newVal)) {
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
    while(dirtyEdges.nonEmpty && steps < maxSteps){
      val (e,lastUpdate) = dirtyEdges.dequeue()
      if(lastUpdate > lastCalibrated(edgeIndex(e))) {
        //recompute
        if(updateEdge(e)) //side-effect
          steps = steps + 1
      }
    }
  }

  override def isConverged: Boolean = dirtyEdges.isEmpty
  override def hasEdge(e: E): Boolean = edges.contains(e)
}
