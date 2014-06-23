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
  def mCompute(ins: IndexedSeq[InEdge#TOut], result: TOut): Unit
  def create: TOut
  def copy(t: TOut): TOut
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

/** Dependently typed partial function. */
trait EdgeValues[-E <: Edge]{
  def hasEdge(e: E): Boolean
  def edgeValue(e: E): e.TOut
}

object EdgeValues {
  def empty = new EdgeValues[Edge]{
    override def hasEdge(e: Edge): Boolean = false
    override def edgeValue(e: Edge): e.type#TOut = sys.error("querying non-existent edge value")
  }
}

trait Calibrated[E <: Edge] extends EdgeValues[E] {
  def isConverged: Boolean
  def iteration: Int
}

/** Calibrator that supports mutable message updates. */
class MutableFIFOCalibrator[E <: MEdge](differ: Diff[E, E#TOut],
                                        tol: Double = 1e-9,
                                        maxSteps: Int = 1000000,
                                        problem: CProb[E],
                                        initialize: EdgeValues[E] = EdgeValues.empty) extends Calibrated[E]{
  type Out = E#TOut
  val edgeIndex: SIIndex[E] = new SIIndex(problem.edges.toSet)
  //edge index
  type EI = edgeIndex.Idx
  val numEdges: Int = edgeIndex.size
  val edges: IndexedSeq[E] = edgeIndex.elements

  //the predecessors of an edge
  val predecessors: IndexedSeq[Array[EI]] = edgeIndex.createArrayLookup(_.inputs.map(_.asInstanceOf[E]))

  //the successors of an edge
  val successors: IndexedSeq[Array[EI]] = {
    //if edges is closed, the following cast succeeds (because E is a valid super type)
    val fromTo: IndexedSeq[(EI, EI)] = for(toI <- 0 until numEdges; fromI <- predecessors(toI)) yield (fromI,toI)
    ((0 until numEdges) map fromTo.groupByMap(_._1,_._2).withDefaultValue(IndexedSeq())).map(_.toArray)
  }

  //the current values of the edges
  private val state: mutable.Buffer[Out] = edges.map(e =>
    if(initialize.hasEdge(e)) e.copy(initialize.edgeValue(e))
    else problem.init(e)
  ).toBuffer

  //object pool for calculating new values
  private val pool: mutable.Buffer[Out] = edges.map(_.create).toBuffer

  //when was an edge calibrated the last time?
  private val lastCalibrated: Array[Int] = Array.fill(numEdges)(-1)

  //TODO optimization Could be replaced by parallel arrays
  private val dirtyEdges: mutable.Queue[(EI,Int)] = mutable.Queue((for(e <- 0 until numEdges) yield e -> 0): _*)

  private var steps: Int = 0

  calibrate()

  /** Assumes that `e` is already dequeued, and steps is incremented. */
  private def updateEdge(ei: Int): Boolean = {
    val e: E = edgeIndex.backward(ei)
    val input: IndexedSeq[e.InEdge#TOut] = predecessors(ei).map(i => state(i).asInstanceOf[e.InEdge#TOut])
    val newVal = pool(ei).asInstanceOf[e.TOut]
    val oldVal = state(ei).asInstanceOf[e.TOut]
    //recalculate
    e.mCompute(input,newVal)
    val diff = differ.diff(e)(oldVal, newVal)
    if(diff > tol) {
      //save new state
      pool(ei) = oldVal
      state(ei) = newVal
      //awake dependent edges
      dirtyEdges.enqueue(successors(ei).map(e => e -> steps):_*)
      lastCalibrated(ei) = steps
      true
    }
    else false
  }

  def calibrate(): Unit = {
    while(dirtyEdges.nonEmpty && steps < maxSteps){
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
  def edgeValue(n: E): n.TOut = state(edgeIndex(n)).asInstanceOf[n.TOut]
  def hasEdge(e: E): Boolean = edgeIndex.contains(e)
  def isConverged = dirtyEdges.isEmpty
}