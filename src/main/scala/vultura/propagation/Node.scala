package vultura.propagation

/** A Node represents one value (container) of type `Type`.
  * An example can be the variable belief for a certain variable, which has Type `Factor`. */
trait Node {
  type Type
}

/** A RepNode knows how to represent a value or type `Type` using type `R`.*/
trait RepNode[R]{self: Node =>
  def construct: R
  def store(t: Type, r: R): Unit
  def load(r: R): Type
}

trait NodeAD extends Node with RepNode[Array[Double]]
