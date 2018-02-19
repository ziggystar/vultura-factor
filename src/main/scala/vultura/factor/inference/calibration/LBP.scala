package vultura.factor.inference.calibration

import vultura.factor._

import scala.collection.mutable

@deprecated("use vultura.inference.BeliefPropagation")
case class LBP(problem: Problem) {

  sealed trait BPMessage extends MEdge {self: Product =>
    def v: Int
    def fi: Int
    def f: Factor = problem.factors(fi)
    final type TOut = Array[Double]
    def create: TOut = new Array[Double](problem.domains(v))
    override def copy(t: TOut): TOut = t.clone()
    override def prettyPrint(t: Array[Double]): String = t.map(_.formatted("%.3f")).mkString(",")
  }

  case class V2F(v: Int, fi: Int) extends BPMessage {
    override type InEdge = F2V
    override def inputs: IndexedSeq[InEdge] = for(nfi <- problem.factorIdxOfVariable(v) if nfi != fi) yield F2V(nfi,v)
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

  case class F2V(fi: Int, v: Int) extends BPMessage {
    override type InEdge = V2F
    override def inputs: IndexedSeq[InEdge] = for(nv <- f.variables if nv != v) yield V2F(nv,fi)
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

  val edges: Iterable[BPMessage] = for{
    fi <- 0 until problem.numFactors
    v <- problem.scopeOfFactor(fi)
    edge <- Seq(F2V(fi,v),V2F(v,fi))
  } yield edge

  val maxEntInitializer = new EdgeValues[BPMessage] {
    override def hasEdge(e: BPMessage): Boolean = true
    override def edgeValue(e: BPMessage): e.type#TOut =  Factor.maxEntropy(Array(e.v),problem.domains,problem.ring).values
  }
}

object LBP{
  /** Convenient inference method. */
  def infer(p: Problem, maxIterations: Int = 1000000, tol: Double = 1e-10) = inferWithStats(p, maxIterations, tol)._1

  /** Convenient inference method.
    * @return Second is true if converged, third is number of update steps. */
  def inferWithStats(p: Problem, maxIterations: Int = 1000000, tol: Double = 1e-10): (BPResult,Boolean,Long) = {
    val lbp = LBP(p)
    val cp = new MutableFIFOCalibrator(lbp.edges)(ConvergenceTest.MaxDiff(tol), maxIterations, lbp.maxEntInitializer)
    val ebp = new StoredResult(p,{
      case v2f: BPResult#V2FMsg => cp.edgeValue(lbp.V2F(v2f.vi,v2f.fi))
      case f2v: BPResult#F2VMsg => cp.edgeValue(lbp.F2V(f2v.fi, f2v.vi))
    })
    (ebp,cp.isConverged,cp.iteration)
  }
}


