package vultura.propagation

import vultura.factor.{Ring, SumProductTask, Factor, ProblemStructure}

import scala.reflect.ClassTag

/** Belief Propagation implementation using [[vultura.factor.SumProductTask]].
  * Has to be calibrated using [[RoundRobinAD]].
  * Supports incremental inference.
  *
  * If you don't use incremental inference, you should use [[vultura.factor.inference.calibration.LBP]] instead,
  * which uses the newer calibration framework.
  */
case class BP(ps: ProblemStructure, ring: Ring[Double]){

  //  slightly generic code
  trait FactorNode extends NodeAD {
    type TRep = Factor
    def variables: Array[ps.VI]

    def construct: Array[Double] = new Array[Double](variables.map(ps.domains).product)
    def store(r: TRep, i: TImpl): Unit = System.arraycopy(r.values,0,i,0,i.length)
    def load(i: TImpl): TRep = Factor(variables,i.clone())
  }

  trait ProductRule[T <: FactorNode,F <: FactorNode] extends TypedRule[ADImpl,T,F] {
    override def typedImplementation(t: T): ADImpl#RuleType = productOf(typedDependenciesOf(t),t)
    def productOf(factors: IndexedSeq[FactorNode], result: FactorNode): ADImpl#RuleType = {
      val spt = SumProductTask(result.variables,ps.domains,factors.map(_.variables)(collection.breakOut),ring)
      (ins,r) => {
        spt.sumProduct(ins,r)
        ring.normalizeInplace(r)
      }
    }
  }
  //end generic code

  case class Parameter(fi: ps.FI) extends FactorNode{
    val variables: Array[ps.FI] = ps.scopeOfFactor(fi)
  }
  case class V2F(vi: ps.VI, fi: ps.FI) extends FactorNode {
    val variables: Array[ps.VI] = Array(vi)
  }
  case class F2V(fi: ps.FI, vi: ps.VI) extends FactorNode {
    val variables: Array[ps.VI] = Array(vi)
  }
  case class VBel(vi: ps.VI) extends FactorNode {
    val variables = Array(vi)
  }

  object V2FRule extends ProductRule[V2F,F2V] {
    override def tTag: ClassTag[V2F] = implicitly[ClassTag[V2F]]
    override def typedDependenciesOf(v: V2F): IndexedSeq[F2V] =
      ps.factorIdxOfVariable(v.vi).filterNot(_ == v.fi).map(F2V(_,v.vi))
  }

  object F2VRule extends ProductRule[F2V,FactorNode] {
    override def tTag: ClassTag[F2V] = implicitly[ClassTag[F2V]]
    override def typedDependenciesOf(t: F2V): IndexedSeq[FactorNode] =
      Parameter(t.fi) +: ps.scopeOfFactor(t.fi).filterNot(_ == t.vi).map(V2F(_,t.fi))
  }

  object VBelRule extends ProductRule[VBel,F2V] {
    override def tTag: ClassTag[VBel] = implicitly[ClassTag[VBel]]
    override def typedDependenciesOf(v1: VBel): IndexedSeq[F2V] =
      ps.factorIdxOfVariable(v1.vi).map(F2V(_,v1.vi))
  }

  def calibrationProblem: CP[ADImpl] = CP(ps.variables.map(VBel), V2FRule andThen F2VRule andThen VBelRule)
}
