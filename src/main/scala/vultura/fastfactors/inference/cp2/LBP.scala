package vultura.fastfactors.inference.cp2

import vultura.fastfactors.inference.MargParI
import vultura.fastfactors.{SumProductTask, FastFactor, Problem}

import scala.collection.mutable

case class LBP(problem: Problem) {

  sealed trait BPMessage extends MEdge {
    def v: Int
    final type TOut = Array[Double]
    def create: TOut = new Array[Double](problem.domains(v))
    override def copy(t: TOut): TOut = t.clone()
  }

  case class V2F(v: Int, f: FastFactor) extends BPMessage {
    override type InEdge = F2V
    override def inputs: IndexedSeq[InEdge] = for(nf <- problem.factorsOfVariable(v) if nf != f) yield F2V(nf,v)
    def mCompute() = {
      //this must be lazy, otherwise inputs gets called indefinitely
      val spTask = SumProductTask(
        remainingVars = Array(v),
        domainSizes = problem.domains,
        inputs.map(f2v => Array(f2v.v))(collection.breakOut),
        problem.ring
      )
      (ins: IndexedSeq[Array[Double]], result: Array[Double]) => {
        spTask.sumProduct(ins,result)
        problem.ring.normalizeInplace(result)
      }
    }
  }

  case class F2V(f: FastFactor, v: Int) extends BPMessage {
    override type InEdge = V2F
    override def inputs: IndexedSeq[InEdge] = for(nv <- f.variables if nv != v) yield V2F(nv,f)
    override def mCompute() = {
      val spTask = SumProductTask(
        remainingVars = Array(v),
        domainSizes = problem.domains,
        (inputs.map(v2f => Array(v2f.v))(collection.breakOut) :+ f.variables)(collection.breakOut),
        problem.ring)
      val factorHolder = new mutable.ArraySeq[Array[Double]](f.variables.size)

      (ins: IndexedSeq[Array[Double]], result: Array[Double]) => {
        var i = 0
        while(i < ins.size){
          factorHolder(i) = ins(i)
          i += 1
        }
        factorHolder(i) = f.values

        spTask.sumProduct(factorHolder,result)
        problem.ring.normalizeInplace(result)
      }
    }
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

object LBP{
  /** Convenient inference method. */
  def infer(problem: Problem, maxIterations: Int = 1000000, tol: Double = 1e-10) = {
    val lbp = LBP(problem)
    val cp = new MutableFIFOCalibrator[lbp.BPMessage](MaxDiff, tol, maxIterations, lbp.cp)
    BPResult(problem, (v,f) => FastFactor(Array(v),cp.edgeValue(lbp.V2F(v,f))), (f,v) => FastFactor(Array(v),cp.edgeValue(lbp.F2V(f,v))))
  }
}

case class BPResult(problem: Problem, v2f: (Int,FastFactor) => FastFactor, f2v: (FastFactor,Int) => FastFactor) extends MargParI {
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def variableBelief(vi: Int): FastFactor =
    FastFactor.multiply(problem.ring)(problem.domains)(problem.factorsOfVariable(vi).map(f => f2v(f,vi))).normalize(problem.ring)

  def factorBelief(f: FastFactor): FastFactor =
    FastFactor.multiply(problem.ring)(problem.domains)(f.variables.map(v => v2f(v,f)) :+ f).normalize(problem.ring)

  /** @return Partition function in encoding specified by `ring`. */
  override def logZ: Double = {
    val factorEntropies = problem.factors.map(f => problem.ring.entropy(factorBelief(f).values))
    val variableEntropies = problem.variables.map(v => problem.ring.entropy(variableBelief(v).values) * (1 - problem.degreeOfVariable(v)))
    val factorExpectations = problem.factors.map(f => problem.ring.logExpectation(factorBelief(f).values, f.values))
    factorEntropies.sum + variableEntropies.sum + factorExpectations.sum
  }

  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = math.exp(logZ)
}
