package vultura.propagation

import scala.reflect.ClassTag

/** A dependently-typed assignment of values to nodes. */
trait IValuation[N <: Node]{ outer =>
  def isDefinedAt(n: N): Boolean
  def istore(n: N, r: N#TImpl): Unit
  final def rvaluation[RN <: RNode](implicit ev: RN =:= N): RValuation[RN] = new RValuation[RN] {
    override def isDefinedAt(n: RN): Boolean = outer.isDefinedAt(n)
    override def rval(n: RN): n.TRep = {
      val r = n.construct
      istore(n,r.asInstanceOf[N#TImpl])
      n.load(r)
    }
  }
  def orElse(other: IValuation[N]): IValuation[N] = new IValuation[N] {
    override def isDefinedAt(n: N): Boolean = outer.isDefinedAt(n) || other.isDefinedAt(n)
    override def istore(n: N, r: N#TImpl): Unit = if(outer.isDefinedAt(n)) outer.istore(n,r) else other.istore(n,r)
  }
  def widen[NN <: Node](implicit ct: ClassTag[N]) = new IValuation[NN] {
    override def isDefinedAt(n: NN): Boolean = ct.runtimeClass.isInstance(n) && outer.isDefinedAt(n.asInstanceOf[N])
    override def istore(n: NN, r: NN#TImpl): Unit = {
      outer.istore(n.asInstanceOf[N],r.asInstanceOf[N#TImpl])
    }
  }
}

object IValuation{
  def empty[N <: Node] = new IValuation[N] {
    override def isDefinedAt(n: N): Boolean = false
    override def istore(n: N, r: N#TImpl): Unit = sys.error("the empty IValuation is defined nowhere")
  }
}

trait RValuation[N <: RNode]{ outer =>
  def isDefinedAt(n: N): Boolean
  def rval(n: N): N#TRep
  def toIVal[NN <: N]: IValuation[NN] = new IValuation[NN] {
    override def isDefinedAt(n: NN): Boolean = outer.isDefinedAt(n)
    override def istore(n: NN, r: NN#TImpl): Unit = {
      n.store(outer.rval(n).asInstanceOf[n.TRep],r.asInstanceOf[n.TImpl])
    }
  }
  def widen[NN <: RNode](implicit ct: ClassTag[N]) = new RValuation[NN] {
    override def isDefinedAt(n: NN): Boolean = ct.runtimeClass.isInstance(n) && outer.isDefinedAt(n.asInstanceOf[N])
    override def rval(n: NN): NN#TRep = {
      outer.rval(n.asInstanceOf[N]).asInstanceOf[NN#TRep]
    }
  }
}
