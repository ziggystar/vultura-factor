package vultura.propagation

import gnu.trove.map.hash.TIntObjectHashMap
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

case class CP[N <: Node : ClassTag, Impl](query: Iterable[N], rules: Seq[Rule[N,N,Impl]] = Seq()){
  def nodes: Set[N] = Iterator
    .iterate(query.toSet)(ns => ns ++ ns.flatMap(n => dependenciesOf(n).getOrElse(Seq())))
    .sliding(2).dropWhile(slide => slide(0).size != slide(1).size)
    .next().head
  def dependenciesOf(n: N): Option[IndexedSeq[N]] = rules.find(_.isDefinedAt(n)).map(_.dependencies(n))
  def descendantsOf(n: N): IndexedSeq[N] = nodes.filter(other => dependenciesOf(other).exists(_.contains(n))).toIndexedSeq
  def implementationOf(n: N): Option[Impl] = rules.find(_.isDefinedAt(n)).map(_.implementation(n))
  def addToQuery[N2 >: N <: Node : ClassTag](qs: Iterable[N2]): CP[N2,Impl] = widen[N2].copy(query = query ++ qs)
  def appendRule[N2 >: N <: Node : ClassTag, T <: N2: ClassTag,D <: N2](rule: Rule[T,D,Impl]): CP[N2,Impl] = widen[N2].appendRuleSameType(rule.widen[N2])
  private def appendRuleSameType(rule: Rule[N,N,Impl]) = this.copy(rules = rules :+ rule)
  def widen[N2 >: N <: Node : ClassTag] = CP[N2,Impl](query, rules.map(_.widen[N2]))
}

trait Valuation[N <: Node]{
  def apply(n: N): n.Type
}
object Valuation{
  def constant[N <: Node](x: N#Type) = new Valuation[N]{
    override def apply(n: N): n.Type = x.asInstanceOf[n.Type]
  }
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

object MaxDiff extends Differ[NodeAD,Array[Double]]{
  override def diff(n: NodeAD, oldValue: Array[Double], newValue: Array[Double]): Double =
    oldValue.zip(newValue).map{case (x,y) => math.abs(x - y)}.max
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
  val stateSizes: Array[Int] = state.map(_.length)
  //to retrieve the temporal result array for node at position `i`, look at `tempStorage(tempStorageIndex(i))`
  //this could be optimized by placing the indices into tempStorage into an array (replacing stateSizes)
  private val tempStorage: TIntObjectHashMap[Array[Double]] = {
    val lookup = new TIntObjectHashMap[Array[Double]](20)
    stateSizes.distinct.foreach{ l =>
      lookup.put(l,new Array[Double](l))
    }
    lookup
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
          val node = nodes(i)
          val impl = implementations(i)
          val stateSize: Int = stateSizes(i)
          val newResult = tempStorage.get(stateSize)
          //NPE at this line means we have an invalid parameter node here, which means initialization is broken
          impl.compute(cp.dependenciesOf(node).get.map(n => state(nodesIndex(n)))(collection.breakOut), newResult)
          val diff = differ.diff(node, state(i), newResult)
          if(diff > maxDiff){
            converged = false
            //update value by swapping
            tempStorage.put(stateSize, state(i))
            state(i) = newResult
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




