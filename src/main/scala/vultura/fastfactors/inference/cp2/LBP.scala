package vultura.fastfactors.inference.cp2

import vultura.fastfactors.{SumProductTask, FastFactor, Problem}

case class LBP(problem: Problem) {

  sealed trait BPMessage extends MEdge {
    def v: Int
    final type TOut = Array[Double]
  }

  case class V2F(v: Int, f: FastFactor) extends BPMessage {
    override type InEdge = F2V
    def create: TOut = new Array[Double](problem.domains(v))
    override def inputs: IndexedSeq[InEdge] = for(nf <- problem.factorsOfVariable(v) if nf != f) yield F2V(nf,v)
    //this must be lazy, otherwise inputs gets called indefinitely
    lazy val spTask = SumProductTask(
      remainingVars = Array(v),
      domainSizes = problem.domains,
      inputs.map(f2v => Array(f2v.v))(collection.breakOut),
      problem.ring
    )
    def mCompute(ins: IndexedSeq[Array[Double]], result: Array[Double]): Unit = {
      spTask.sumProduct(ins,result)
      problem.ring.normalizeInplace(result)
    }
  }

  case class F2V(f: FastFactor, v: Int) extends BPMessage {
    override type InEdge = V2F
    def create: TOut = new Array[Double](problem.domains(v))
    override def inputs: IndexedSeq[InEdge] = for(nv <- f.variables if nv != v) yield V2F(nv,f)
    override def mCompute(ins: IndexedSeq[Array[Double]], result: Array[Double]): Unit = {
      spTask.sumProduct(ins :+ f.values,result)
      problem.ring.normalizeInplace(result)
    }
    //this must be lazy, otherwise inputs gets called indefinitely
    lazy val spTask = SumProductTask(
      remainingVars = Array(v),
      domainSizes = problem.domains,
      (inputs.map(v2f => Array(v2f.v))(collection.breakOut) :+ f.variables)(collection.breakOut),
      problem.ring
    )
  }

  val cp =  new CProb[BPMessage]{
    val edges: Iterable[BPMessage] = for{
      f <- problem.factors
      v <- f.variables
      edge <- Seq(F2V(f,v),V2F(v,f))
    } yield edge

    def init(e: BPMessage): Array[Double] = FastFactor.maxEntropy(Array(e.v),problem.domains,problem.ring).values
  }
}
