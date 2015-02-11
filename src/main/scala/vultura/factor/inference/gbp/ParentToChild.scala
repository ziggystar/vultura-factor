package vultura.factor.inference.gbp

import vultura.factor.{ProblemStructure, SumProductTask, Factor, Ring}
import vultura.propagation.{ADImpl, TypedRule, NodeAD}

import scala.reflect.ClassTag

case class ParentToChild(rg: RG, ring: Ring[Double]) {
  type VI = rg.VI
  type FI = rg.FI

  private val ps: ProblemStructure = rg.problemStructure
  val domains = ps.domains

  //  slightly generic code
  trait FactorNode extends NodeAD {
    type TRep = Factor
    def variables: Array[VI]

    def construct: Array[Double] = new Array[Double](variables.map(domains).product)
    def store(r: TRep, i: TImpl): Unit = System.arraycopy(r.values,0,i,0,i.length)
    def load(i: TImpl): TRep = Factor(variables,i.clone())
  }

  trait ProductRule[T <: FactorNode,F <: FactorNode] extends TypedRule[ADImpl,T,F] {
    override def typedImplementation(t: T): ADImpl#RuleType = productOf(typedDependenciesOf(t),t)
    def productOf(factors: IndexedSeq[FactorNode], result: FactorNode): ADImpl#RuleType = {
      val spt = SumProductTask(result.variables,domains,factors.map(_.variables)(collection.breakOut),ring)
      (ins,r) => {
        spt.sumProduct(ins,r)
        ring.normalizeInplace(r)
      }
    }
  }
  //end generic code

  case class Parameter(fi: FI) extends FactorNode {
    override def variables: Array[VI] = ps.scopeOfFactor(fi)
  }
  case class DownEdge(parent: Reg, child: Reg) extends FactorNode{
    override def variables: Array[VI] = child.variables.toArray
  }
  //region belief
  case class RBel(r: Reg) extends FactorNode{
    override def variables: Array[VI] = r.variables.toArray
  }

  object UpdateRule extends ProductRule[DownEdge,DownEdge]{
    override def tTag: ClassTag[DownEdge] = implicitly[ClassTag[DownEdge]]
    override def typedDependenciesOf(t: DownEdge): IndexedSeq[DownEdge] = ???
  }
}
