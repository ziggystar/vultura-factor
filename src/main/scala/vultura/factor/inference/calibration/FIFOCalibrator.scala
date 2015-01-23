package vultura.factor.inference.calibration

import vultura.util._
import vultura.util.graph.DotGraph
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

  type EdgeIndex = Int
  val edges: SIIndex[E] = new SIIndex(problem.toSet)
  def edgeIndex(e: E): EdgeIndex = edges.forward(e)

  //when a node changes its state, this tells us which edges need to be recomputed
  val dependentEdges: IndexedSeq[Array[EdgeIndex]] = {
    val broken: IndexedSeq[(E, E)] = for(e <- edges.elements; in <- e.inputs) yield in.asInstanceOf[E] -> e
    val m: Map[E, IndexedSeq[E]] = broken.groupByMap(_._1,_._2).withDefaultValue(IndexedSeq())
    edges.createArrayLookup(m)
  }

  private val state: mutable.Buffer[Any] = edges.elements.map(e => initialize.edgeValue(e)).toBuffer

  //when was an edge calibrated the last time?
  private val lastCalibrated: Array[Long] = Array.fill(edges.elements.size)(-1)

  private val dirtyEdges: MutableArrayQueue[(Int, Long)] = vultura.util.MutableArrayQueue(for(e <- edges.indices) yield e -> 0L)

  private var steps: Long = 0

  def iteration: Long = steps

  def edgeValue(n: E): n.TOut = state(edgeIndex(n)).asInstanceOf[n.TOut]

  val inputMemo: IndexedSeq[Array[EdgeIndex]] = {
    val m: Map[E,IndexedSeq[E]] = edges.elements.map(e => e -> e.inputs.map(_.asInstanceOf[E]))(collection.breakOut)
    edges.createArrayLookup(m)
  }

  calibrate()

  /** Assumes that `e` is already dequeued, and steps is incremented.
    * @return True if the edge has been updated. */
  private def updateEdge(ei: Int): Boolean = {
    val e: E = edges.backward(ei)
    val input: IndexedSeq[e.InEdge#TOut] = inputMemo(ei).map(n => state(n).asInstanceOf[e.InEdge#TOut])(collection.breakOut)
    //recalculate
    val newVal: e.TOut = e.compute(input)
    if(!convergenceTest.isConverged(e)(state(ei).asInstanceOf[e.TOut],newVal)) {
      //save new state
      state(ei) = newVal
      //awake dependent edges
      dependentEdges(ei).foreach(di => dirtyEdges.enqueue(di -> steps))
      lastCalibrated(ei) = steps
      steps = steps + 1
      true
    }
    else false
  }

  def calibrate(): Unit = {
    while(dirtyEdges.nonEmpty && steps < maxSteps){
      val (ei,lastUpdate) = dirtyEdges.dequeue()
      if(lastUpdate > lastCalibrated(ei)) {
        //recompute, side-effect
        updateEdge(ei)
      }
    }
  }

  override def isConverged: Boolean = dirtyEdges.isEmpty
  override def hasEdge(e: E): Boolean = edges.contains(e)

  def toDot: DotGraph[Int] = {
    val g = DotGraph(
      edges = for{
        (preds,ei) <- inputMemo.zipWithIndex
        pred <- preds
      } yield pred -> ei,
      graphName = "MutableFIFOCalibrator"
    ).nodeLabeled { ei =>
      val edge: E = edges.backward(ei)
      s"$edge\\n ${edge.prettyPrint(state(ei).asInstanceOf[edge.TOut])}"
    }
    g.copy(nodeOptions = g.nodeOptions :+ ((i: Int) => edges.backward(i).dotNodeOption.mkString(",")))
  }

  def toCSV: String =
    edges.backwardMap.zip(state)
      .sortBy(_._1.toString)
      .map(t => t._1 + "\t" + t._1.prettyPrint(t._2.asInstanceOf[t._1.TOut]))
      .mkString("\n")
}
