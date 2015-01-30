package vultura.propagation

import vultura.factor.{Ring, SumProductTask, Factor, ProblemStructure}

case class BP(ps: ProblemStructure, ring: Ring[Double]){

  //  slightly generic code
  trait FactorNode {self: NodeAD =>
    type Type = Factor
    def variables: Array[ps.VI]

    def construct: Array[Double] = new Array[Double](variables.map(ps.domains).product)
    def store(t: Factor, r: Array[Double]): Unit = System.arraycopy(t.values,0,r,0,r.size)
    def load(r: Array[Double]): Factor = Factor(variables,r.clone())
  }

  trait ProductRule[T <: FactorNode,F <: FactorNode] {self: RuleAD[T,F] =>
    override def implementation(v: T): ImplAD = productOf(self.dependencies(v),v)

    def productOf(factors: IndexedSeq[FactorNode], result: FactorNode): ImplAD = new ImplAD {
      val spt = SumProductTask(result.variables,ps.domains,factors.map(_.variables)(collection.breakOut),ring)
      def compute: (Array[Array[Double]], Array[Double]) => Unit = (ins,r) => {
        spt.sumProduct(ins,r)
        ring.normalizeInplace(r)
      }
    }
  }
  //end generic code

  trait BPNode extends NodeAD
  trait BPFactorNode extends BPNode with FactorNode
  
  case class Parameter(fi: ps.FI) extends BPFactorNode{
    val variables: Array[ps.FI] = ps.scopeOfFactor(fi)
  }
  case class V2F(vi: ps.VI, fi: ps.FI) extends BPFactorNode {
    val variables: Array[ps.VI] = Array(vi)
  }
  case class F2V(fi: ps.FI, vi: ps.VI) extends BPFactorNode {
    val variables: Array[ps.VI] = Array(vi)
  }
  case class VBel(vi: ps.VI) extends BPFactorNode {
    val variables = Array(vi)
  }

  object V2FRule extends RuleAD[V2F,F2V] with ProductRule[V2F,F2V] {
    override def dependencies(v: V2F): IndexedSeq[F2V] = ps.factorIdxOfVariable(v.vi).filterNot(_ == v.fi).map(F2V(_,v.vi))
  }

  object F2VRule extends RuleAD[F2V,BPFactorNode] with ProductRule[F2V,BPFactorNode] {
    override def dependencies(v1: F2V): IndexedSeq[BPFactorNode] = Parameter(v1.fi) +: ps.scopeOfFactor(v1.fi).filterNot(_ == v1.vi).map(V2F(_,v1.fi))
  }

  object VBelRule extends RuleAD[VBel,F2V] with ProductRule[VBel,F2V] {
    override def dependencies(v1: VBel): IndexedSeq[F2V] = ps.factorIdxOfVariable(v1.vi).map(F2V(_,v1.vi))
  }

  def calibrationProblem: CP[BPNode, ImplAD] =
    CP(ps.variables.map(VBel)) appendRule V2FRule appendRule F2VRule appendRule VBelRule
}
