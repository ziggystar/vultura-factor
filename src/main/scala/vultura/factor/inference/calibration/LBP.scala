package vultura.factor.inference.calibration

import vultura.factor.inference.MargParI
import vultura.factor.{Factor, Problem, SumProductTask}

import scala.collection.mutable

case class LBP(problem: Problem) {

  sealed trait BPMessage extends MEdge {
    def v: Int
    def f: Factor
    final type TOut = Array[Double]
    def create: TOut = new Array[Double](problem.domains(v))
    override def copy(t: TOut): TOut = t.clone()
  }

  case class V2F(v: Int, f: Factor) extends BPMessage {
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

  case class F2V(f: Factor, v: Int) extends BPMessage {
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

  val cp =  new CalibrationProblem[BPMessage]{
    val edges: Iterable[BPMessage] = for{
      f <- problem.factors
      v <- f.variables
      edge <- Seq(F2V(f,v),V2F(v,f))
    } yield edge

    def init(e: BPMessage): Array[Double] = Factor.maxEntropy(Array(e.v),problem.domains,problem.ring).values
  }
}

object LBP{
  /** Convenient inference method. */
  def infer(p: Problem, maxIterations: Int = 1000000, tol: Double = 1e-10) = {
    val lbp = LBP(p)
    val cp = new MutableFIFOCalibrator(lbp.cp)(MaxDiff, tol, maxIterations)
    new BPResult{
      override def v2f(m: (Int, Factor)): Factor = Factor(Array(m._1),cp.edgeValue(lbp.V2F(m._1,m._2)))
      override def f2v(m: (Factor, Int)): Factor = Factor(Array(m._2),cp.edgeValue(lbp.F2V(m._1,m._2)))
      override def problem: Problem = p
    }
  }
}

/** A mixin to calculate variable beliefs and the log partition function from BP messages. */
trait BPResult extends MargParI {
  def v2f(m: (Int,Factor)): Factor
  def f2v(m: (Factor,Int)): Factor
  def problem: Problem
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def variableBelief(vi: Int): Factor =
    Factor.multiply(problem.ring)(problem.domains)(problem.factorsOfVariable(vi).map(f => f2v(f,vi))).normalize(problem.ring)

  def factorBelief(f: Factor): Factor =
    Factor.multiply(problem.ring)(problem.domains)(f.variables.map(v => v2f(v,f)) :+ f).normalize(problem.ring)

  /** This is lazy to prevent accessing elements in derived classes early.
   * @return Partition function in encoding specified by `ring`. */
  override lazy val logZ: Double = {
    var logExp: Double = 0d
    var factorEntropy = 0d
    var variableEntropy = 0d

    //variable entropies
    {
      var i = 0
      while (i < problem.numVariables) {
        val vb: Array[Double] = variableBelief(i).values
        val entropy: Double = problem.ring.entropy(vb)
        //check whether we are inconsistent
        if(entropy == 0d && problem.ring.sumA(vb) == problem.ring.zero)
          logExp = Double.NegativeInfinity
        variableEntropy = variableEntropy + entropy * (1 - problem.degreeOfVariable(i))
        i = i + 1
      }
    }

    {
      //factor entropies and factor log-expectations
      var i = 0
      while (i < problem.factors.length) {
        val f = problem.factors(i)
        val fb: Array[Double] = factorBelief(f).values
        factorEntropy = factorEntropy + problem.ring.entropy(fb)
        logExp = logExp + problem.ring.logExpectation(fb, f.values)
        i += 1
      }
    }

    val result = logExp + factorEntropy + variableEntropy
    result
  }

  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = math.exp(logZ)
}
