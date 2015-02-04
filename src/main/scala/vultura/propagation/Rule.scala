package vultura.propagation

import scala.reflect.ClassTag

/** A node knows its representation type `TImpl`.
  * This cannot happen via type class, since then it will be difficult to assert that the relation between
  * node type and representation type is indeed a function.
  * Also a node knows how to construct empty representation values that are able to hold the information about the node.
  * The implementation type `TImpl` is a mutable type.
  */
trait Node {
  type TImpl
  def construct: TImpl
}

/** An rnode is a node that has a representation type, usually a nicer high-level representation of a node value.
  * The representation type `TRep` should be an immutable type. */
trait RNode extends Node {
  type TRep
  def store(r: TRep, i: TImpl): Unit
  def load(i: TImpl): TRep
}

/** A node that uses arrays of doubles as its internal representation. */
trait NodeAD extends Node with RNode{
  type TImpl  = Array[Double]
}

/** A rule defines a DAG over a nodes of type `N`.
  * It also provides a calculation rule `Impl`.
  * The type of the implementation rule must match the representation type of `N`. */
trait Rule[Impl <: Implementation]{ outer =>
  type N = Impl#NodeType
  def isDefinedAt(n: N): Boolean = true
  def dependenciesOf(n: N): IndexedSeq[N]
  def implementation(n: N): Impl#RuleType
  /** Compose two rules, where this rule has precedence over rule `r`. */
  def andThen(r: Rule[Impl]): Rule[Impl] = new Rule[Impl]{
    override def isDefinedAt(n: N): Boolean = outer.isDefinedAt(n) || r.isDefinedAt(n)
    override def dependenciesOf(n: N): IndexedSeq[N] =
      if(outer.isDefinedAt(n)) outer.dependenciesOf(n) else r.dependenciesOf(n)
    override def implementation(n: N): Impl#RuleType =
      if(outer.isDefinedAt(n)) outer.implementation(n) else r.implementation(n)
  }
}

/** An implementation defines a type for the rule computation, and places constraints on the compatible nodes. */
trait Implementation {
  type NodeType <: Node
  type RuleType
}

trait ADImpl extends Implementation {
  type NodeType = NodeAD
  type RuleType = (Array[Array[Double]],Array[Double]) => Unit
}

/** A rule with an implementation using arrays of Doubles. */
trait RuleAD extends Rule[ADImpl]

trait TypedRule[Impl <: Implementation, T <: Impl#NodeType, +D <: Impl#NodeType] extends Rule[Impl]{
  override final def isDefinedAt(n: N): Boolean = tTag.runtimeClass.isInstance(n) && typedIsDefinedAt(n.asInstanceOf[T])
  override final def dependenciesOf(n: N): IndexedSeq[N] = typedDependenciesOf(n.asInstanceOf[T])
  override final def implementation(n: N): Impl#RuleType = typedImplementation(n.asInstanceOf[T])

  def tTag: ClassTag[T]
  def typedIsDefinedAt(t: T): Boolean = true
  def typedDependenciesOf(t: T): IndexedSeq[D]
  def typedImplementation(t: T): Impl#RuleType
}