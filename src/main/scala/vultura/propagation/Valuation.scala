package vultura.propagation

/** A dependently-typed assignment of values to nodes. */
trait Valuation[N <: Node]{
  def apply(n: N): n.Type
}

object Valuation{
  /** The valuation of constant value `x`. */
  def constant[N <: Node](x: N#Type) = new Valuation[N]{
    override def apply(n: N): n.Type = x.asInstanceOf[n.Type]
  }
}
trait PartialValuation[N <: Node]{
  def apply(n: N): Option[n.Type]
}
