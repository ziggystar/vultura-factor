package vultura.factor.inference.calibration

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
  /** These computations don't have to be thread-safe. */
  def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit
  def create: TOut
  def copy(t: TOut): TOut
  override def compute(ins: IndexedSeq[InEdge#TOut]): TOut = {
    val c = create
    mCompute()(ins,c)
    c
  }
}

/** Trait for a calibration problem. */
trait CalibrationProblem[E <: Edge]{
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
  def iteration: Long
}