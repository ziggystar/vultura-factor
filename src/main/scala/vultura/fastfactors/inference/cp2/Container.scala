package vultura.fastfactors.inference.cp2

import scala.collection.mutable
import vultura.util._

trait Edge {
  //the type of the parent edges
  type InEdge <: Edge
  //the type that is produced by this edge
  type TOut <: AnyRef
  def inputs: IndexedSeq[InEdge]
  def compute(ins: IndexedSeq[InEdge#TOut]): TOut
}

/** Mutable edge type.
  * Is able to write the result directly into a provided container.
  */
trait MEdge extends Edge {
  /** Mutable container type. */
  def mCompute(ins: IndexedSeq[InEdge#TOut], result: TOut): Unit
  def create: TOut
  override def compute(ins: IndexedSeq[InEdge#TOut]): TOut = {
    val c = create
    mCompute(ins,c)
    c
  }
}

trait Diff[-E,-T]{
  def diff(e: E)(oldValue: T, newValue: T): Double
}

object MaxDiff extends Diff[Any,Array[Double]]{
  override def diff(e: Any)(oldValue: Array[Double], newValue: Array[Double]): Double =
    (for((v1,v2) <- oldValue zip newValue) yield math.abs(v1-v2)).max
}

trait CProb[E <: Edge]{
  def edges: Iterable[E]
  def init(e: E): e.TOut
}

class MutableFIFOCalibrator[E <: MEdge](differ: Diff[E, E#TOut],
                                        tol: Double = 1e-9,
                                        maxSteps: Int = 1000000)(problem: CProb[E]){
  //edge index
  type Out = E#TOut
  type EI = Int
  val edgeList: IndexedSeq[E] = problem.edges.toIndexedSeq.distinct
  val numEdges = edgeList.size
  val edgeIndex: collection.Map[E, EI] = mutable.HashMap(edgeList.zipWithIndex:_*)

  //the predecessors of an edge
  val predecessors: IndexedSeq[Array[EI]] =
    edgeList.map(_.inputs.map(e => edgeIndex(e.asInstanceOf[E]))(collection.breakOut): Array[EI])

  //the successors of an edge
  val successors: IndexedSeq[Array[EI]] = {
    //if edges is closed, the following cast succeeds (because E is a valid super type)
    val fromTo: IndexedSeq[(EI, EI)] = for(toI <- 0 until numEdges; fromI <- predecessors(toI)) yield (fromI,toI)
    ((0 until numEdges) map fromTo.groupByMap(_._1,_._2).withDefaultValue(IndexedSeq())).map(_.toArray)
  }

  //the current values of the edges
  private val state: mutable.Buffer[Out] = edgeList.map(e => problem.init(e)).toBuffer
  //object pool for calculating new values
  private val pool: mutable.Buffer[Out] = edgeList.map(_.create).toBuffer

  //when was an edge calibrated the last time?
  private val lastCalibrated: Array[Int] = Array.fill(edgeList.size)(-1)

  //TODO optimization Could be replaced by parallel arrays
  private val dirtyEdges: mutable.Queue[(EI,Int)] = mutable.Queue((for(e <- 0 until numEdges) yield e -> 0): _*)

  private var steps: Int = 0

  calibrate()

  /** Assumes that `e` is already dequeued, and steps is incremented. */
  private def updateEdge(edgeIndex: Int): Boolean = {
    val e = edgeList(edgeIndex)
    val input: IndexedSeq[e.InEdge#TOut] = predecessors(edgeIndex).map(i => state(i).asInstanceOf[e.InEdge#TOut])
    val newVal = pool(edgeIndex).asInstanceOf[e.TOut]
    val oldVal = state(edgeIndex).asInstanceOf[e.TOut]
    //recalculate
    e.mCompute(input,newVal)
    val diff = differ.diff(e)(oldVal, newVal)
    if(diff > tol) {
      //save new state
      pool(edgeIndex) = oldVal
      state(edgeIndex) = newVal
      //awake dependent edges
      dirtyEdges.enqueue(successors(edgeIndex).map(e => e -> steps):_*)
      lastCalibrated(edgeIndex) = steps
      true
    }
    else false
  }

  def calibrate(): Unit = {
    while(!dirtyEdges.isEmpty && steps < maxSteps){
      val (ei,lastUpdate) = dirtyEdges.dequeue()
      if(lastUpdate > lastCalibrated(ei)) {
        //recompute
        val wasChanged: Boolean = updateEdge(ei)
        if(wasChanged) //side-effect
          steps = steps + 1
      }
    }
  }

  def iteration: Int = steps
  def nodeState(n: E): n.TOut = state(edgeIndex(n)).asInstanceOf[n.TOut]
  def isCalibrated = dirtyEdges.isEmpty
}