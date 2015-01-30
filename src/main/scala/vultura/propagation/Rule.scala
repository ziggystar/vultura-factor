package vultura.propagation

import scala.reflect.ClassTag

/** Implements a rule with storage `R`. */
trait RuleImplM[R] {
  /** First contains the storage for the rule's dependencies,
    * second contains the storage where the result shall be put. */
  def compute: (Array[R],R) => Unit
}

/** Shorthand for `Array[Double]` backed storage. */
trait ImplAD extends RuleImplM[Array[Double]]

trait RuleAD[T,D] extends Rule[T,D,ImplAD]

/** A rule, computing values for `T` from dependencies of type `D`.
  * Implementation must overwrite either `apply` or `dependencies` and `implementation`.
  * @tparam T Node type for computed node.
  * @tparam D Type of nodes the computation depends on.
  * @tparam Impl The type of the provided implementation.
  */
trait Rule[T,+D,Impl] extends PartialFunction[T,(IndexedSeq[D],Impl)]{ outer =>
  /** Use this to define a partial rule. */
  def isDefinedAt(x: T): Boolean = true
  def apply(v1: T): (IndexedSeq[D], Impl) = (dependencies(v1), implementation(v1))
  def dependencies(v1: T): IndexedSeq[D] = apply(v1)._1
  def implementation(v1: T): Impl = apply(v1)._2

  /** Upcast the rule to apply to a more general type. */
  final def widen[U: ClassTag](implicit tag: ClassTag[T]): Rule[U,D,Impl] =
    if(implicitly[ClassTag[U]] == tag) outer.asInstanceOf[Rule[U,D,Impl]]
    else new Rule[U,D,Impl] {
      def cast(u: U): Option[T] = u match {
        case tag(t) => Some(t)
        case _ => None
      }
      override def isDefinedAt(x: U): Boolean = cast(x).exists(outer.isDefinedAt)
      override def apply(v1: U): (IndexedSeq[D], Impl) = outer(cast(v1).get)
    }
}

trait Calibrated[N <: Node] extends Valuation[N]{
  def totalUpdates: Long
  def isConverged: Boolean
}

trait Calibrator[Impl]{
  def calibrate[N <: Node](cp: CP[N,Impl]): Calibrated[N]
}




