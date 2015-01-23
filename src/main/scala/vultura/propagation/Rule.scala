package vultura.propagation

import gnu.trove.map.hash.TIntObjectHashMap
import vultura.util.SIIndex

import scala.reflect.ClassTag

/** Implements a rule with implementation type `R`. */
trait RuleImplM[R] {
  def compute: (Array[R],R) => Unit
}

trait ImplAD extends RuleImplM[Array[Double]]

trait Rule[T,+D,Impl] extends PartialFunction[T,(IndexedSeq[D],Impl)]{ outer =>
  def isDefinedAt(x: T): Boolean = true
  def apply(v1: T): (IndexedSeq[D], Impl) = (dependencies(v1), implementation(v1))
  def dependencies(v1: T): IndexedSeq[D] = apply(v1)._1
  def implementation(v1: T): Impl = apply(v1)._2

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

trait RuleAD[T,D] extends Rule[T,D,ImplAD]

trait Representable[@specialized (Double) T, R]{
  def represent(t: T): R
  def reify(a: R): T
}

trait RepresentableAD[@specialized (Double) T] extends Representable[T,Array[Double]]

trait Node {
  type Type
  type R
  def construct: R
  def store(t: Type, r: R): Unit
  def load(r: R): Type
}

trait NodeAD extends Node {
  type R = Array[Double]
}

trait Valuation[N <: Node]{
  def apply(n: N): n.Type
}

case class CP[N <: Node : ClassTag, Impl](query: Iterable[N], rules: Seq[Rule[N,N,Impl]]){
  def nodes: Set[N] = Iterator
    .iterate(query.toSet)(ns => ns.flatMap(n => dependenciesOf(n).getOrElse(Seq())))
    .sliding(2).dropWhile(slide => slide(0).size != slide(1).size)
    .next().head
  def dependenciesOf(n: N): Option[IndexedSeq[N]] = rules.find(_.isDefinedAt(n)).map(_.dependencies(n))
  def implementationOf(n: N): Option[Impl] = rules.find(_.isDefinedAt(n)).map(_.implementation(n))
  def addToQuery[N2 >: N <: Node : ClassTag](qs: Iterable[N2]): CP[N2,Impl] = widen[N2].copy(query = query ++ qs)
  def appendRule[N2 >: N <: Node : ClassTag, T <: N2,D <: N2](rule: Rule[T,D,Impl]): CP[N2,Impl] = widen[N2].appendRule(rule)
  private def appendRuleSameType(rule: Rule[N,N,Impl]) = this.copy(rules = rules :+ rule)
  def widen[N2 >: N <: Node : ClassTag] = CP[N2,Impl](query, rules.map(_.widen[N2]))
}

trait Calibrated[N <: Node] {
  def getValue(n: N): n.Type
  def totalUpdates: Long
  def isConverged: Boolean
}

trait Differ[-N <: Node, -R]{
  def diff(n: N, oldValue: R, newValue: R): Double
}
trait Calibrator[Impl]{
  def calibrate[N <: Node](cp: CP[N,Impl]): Calibrated[N]
}
class RoundRobinAD[N <: NodeAD](cp: CP[N, ImplAD], differ: Differ[N,Array[Double]]){
  private val nodes = new SIIndex(cp.nodes)
  private val state: Array[Array[Double]] = ???
  private val tempStorage: TIntObjectHashMap[Array[Double]] = new TIntObjectHashMap[Array[Double]](20)
  /** Those nodes that have to be provided with an initial value. */
  def inputs: IndexedSeq[N]
  def calibrate(params: N => Array[Double], maxDiff: Double, maxSteps: Long): Unit = ???
}




