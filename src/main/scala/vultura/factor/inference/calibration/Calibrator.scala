package vultura.factor.inference.calibration

import vultura.util.HashMemo

trait Edge extends HashMemo {self: Product =>
  //the type of the parent edges
  type InEdge <: Edge
  //the type that is produced by this edge
  type TOut <: Any
  def inputs: IndexedSeq[InEdge]
  def compute(ins: IndexedSeq[InEdge#TOut]): TOut
  def prettyPrint(t: TOut): String = t.toString
  def dotNodeOption: Seq[String] = Seq()
}

object Edge{
  def expand[U <: Edge](xs: U*): Set[U] = {
    def expandR(edges: Set[U], closed: Set[U] = Set()): Set[U] = if (edges.isEmpty) closed
    else {
      val newClosed = edges ++ closed
      val preds: Set[U] = (for {
        e <- edges
        in <- e.inputs.map(_.asInstanceOf[U]) if !newClosed(in)
      } yield in)(collection.breakOut)
      expandR(preds, newClosed)
    }
    expandR(xs.toSet)
  }
}

/** Mutable edge type.
  * Is able to write the result directly into a provided container.
  */
trait MEdge extends Edge {self: Product =>
  /** These computations don't have to be thread-safe. */
  def mCompute(): (IndexedSeq[InEdge#TOut], TOut) => Unit
  /** Only has to create the value container (e.g. an array); the value will be taken from elsewhere. */
  def create: TOut
  def copy(t: TOut): TOut
  override def compute(ins: IndexedSeq[InEdge#TOut]): TOut = {
    val c = create
    mCompute()(ins,c)
    c
  }
}

/** Dependently typed partial function. */
trait EdgeValues[-E <: Edge]{outer =>
  def hasEdge(e: E): Boolean
  def edgeValue(e: E): e.TOut
  def orElse[EE <: E](other: EdgeValues[EE]) = new EdgeValues[EE]{
    override def hasEdge(e: EE): Boolean = outer.hasEdge(e) || other.hasEdge(e)

    override def edgeValue(e: EE): e.type#TOut = if(outer.hasEdge(e)) outer.edgeValue(e) else other.edgeValue(e)
  }
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