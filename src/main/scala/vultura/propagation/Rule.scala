package vultura.propagation

import vultura.util.SIIndex

import scala.collection.mutable
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
}

trait RepNode[R]{self: Node =>
  def construct: R
  def store(t: Type, r: R): Unit
  def load(r: R): Type
}

trait NodeAD extends Node with RepNode[Array[Double]]

case class CP[N <: Node : ClassTag, Impl](query: Iterable[N], rules: Seq[Rule[N,N,Impl]]){
  def nodes: Set[N] = Iterator
    .iterate(query.toSet)(ns => ns.flatMap(n => dependenciesOf(n).getOrElse(Seq())))
    .sliding(2).dropWhile(slide => slide(0).size != slide(1).size)
    .next().head
  def dependenciesOf(n: N): Option[IndexedSeq[N]] = rules.find(_.isDefinedAt(n)).map(_.dependencies(n))
  def descendantsOf(n: N): IndexedSeq[N] = nodes.filter(other => dependenciesOf(other).exists(_.contains(n))).toIndexedSeq
  def implementationOf(n: N): Option[Impl] = rules.find(_.isDefinedAt(n)).map(_.implementation(n))
  def addToQuery[N2 >: N <: Node : ClassTag](qs: Iterable[N2]): CP[N2,Impl] = widen[N2].copy(query = query ++ qs)
  def appendRule[N2 >: N <: Node : ClassTag, T <: N2,D <: N2](rule: Rule[T,D,Impl]): CP[N2,Impl] = widen[N2].appendRule(rule)
  private def appendRuleSameType(rule: Rule[N,N,Impl]) = this.copy(rules = rules :+ rule)
  def widen[N2 >: N <: Node : ClassTag] = CP[N2,Impl](query, rules.map(_.widen[N2]))
}

trait Valuation[N <: Node]{
  def apply(n: N): n.Type
}
trait PartialValuation[N <: Node]{
  def apply(n: N): Option[n.Type]
}
trait Calibrated[N <: Node] extends Valuation[N]{
  def totalUpdates: Long
  def isConverged: Boolean
}

trait Differ[-N <: Node, -R]{
  def diff(n: N, oldValue: R, newValue: R): Double
}

trait Calibrator[Impl]{
  def calibrate[N <: Node](cp: CP[N,Impl]): Calibrated[N]
}

class RoundRobinAD[N <: Node with RepNode[Array[Double]]](cp: CP[N, ImplAD],
                                                          differ: Differ[N,Array[Double]],
                                                          initializer: Valuation[N]){
  private val nodesIndex: SIIndex[N] = new SIIndex(cp.nodes)
  private val nodes = nodesIndex.elements
  val implementations = nodes.map(n => cp.implementationOf(n).orNull)
  private val state: Array[Array[Double]] = nodes.map(_.construct)(collection.breakOut)
  //at position i, holds a spare array with the size for representing node(i)
  private val tempStorage: Array[Array[Double]] = {
    val lookup = new mutable.HashMap[Int,Array[Double]]()
    state.map(a => lookup.getOrElseUpdate(a.size,new Array[Double](a.size)))
  }
  /** Those nodes that have to be provided with an initial value. */
  def inputNodes: IndexedSeq[N] = nodes.filterNot(n => cp.implementationOf(n).isDefined)

  def calibrate(params: Valuation[N],
                maxDiff: Double = 1e-9,
                maxLoops: Long = 1000,
                specificInitializer: Option[PartialValuation[N]] = None): Calibrated[N] = {
    val isValid: mutable.BitSet = new mutable.BitSet(nodes.size)
    //initialize the nodes
    nodes.zip(state).zipWithIndex.foreach{case ((n,ad),i) =>
        if(cp.implementationOf(n).isDefined) {
          n.store(specificInitializer.flatMap(_.apply(n)).getOrElse(initializer(n)), ad)
          isValid(i) = false
        } else {
          n.store(params(n), ad)
          isValid(i) = true
        }
    }

    //run the calibration loop
    var converged = false
    var loops = 0
    while(loops < maxLoops && !converged) {
      loops += 1
      converged = true

      var i = 0
      while (i < nodes.size) {
        if (!isValid(i)) {
          converged = false
          val node = nodes(i)
          val impl = implementations(i)
          val tempArray = tempStorage(i)
          //NPE at this line means we have an invalid parameter node here, which means initialization is broken
          impl.compute(cp.dependenciesOf(node).get.map(n => state(nodesIndex(n)))(collection.breakOut), tempArray)
          val diff = differ.diff(node, state(i), tempArray)
          if(diff < maxDiff){
            //invalidate descendants
            for(desc <- cp.descendantsOf(node)){
              isValid(nodesIndex(desc)) = false
            }
          }
          isValid(i) = true
        }
        i += 1
      }
    }
    new Calibrated[N] {
      val myState = state.clone()
      override def totalUpdates: Long = loops
      override def isConverged: Boolean = converged
      override def apply(n: N): n.Type = n.load(myState(nodesIndex(n)))
    }
  }
}




