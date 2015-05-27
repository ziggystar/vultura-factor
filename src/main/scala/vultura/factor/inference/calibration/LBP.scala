package vultura.factor.inference.calibration

import vultura.factor.inference.{JointMargI, MargParI}
import vultura.factor.{Var, Factor, Problem, SumProductTask}

import scala.collection.mutable

case class LBP(problem: Problem) {

  sealed trait BPMessage extends MEdge {self: Product =>
    def v: Int
    def fi: Int
    def f: Factor = problem.factors(fi)
    final type TOut = Array[Double]
    def create: TOut = new Array[Double](problem.domains(v))
    override def copy(t: TOut): TOut = t.clone()
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

/** A mixin to calculate variable beliefs and the log partition function from BP messages. */
trait BPResult extends MargParI with JointMargI {
  sealed trait Message{
    def vi: Int
    def fi: Int

    def variables: Array[Int] = Array(vi)
    def factorScope: Array[Int] = problem.scopeOfFactor(fi)
    lazy val varIndexInFactorScope: Int = factorScope.indexOf(vi)
  }
  case class V2FMsg(vi: Int, fi: Int) extends Message
  case class F2VMsg(fi: Int, vi: Int) extends Message

  def rawMessageValue(m: Message): Array[Double]

  def messageValue(m: Message): Factor = Factor(m.variables,rawMessageValue(m))

  def problem: Problem
  /** @return marginal distribution of variable in encoding specified by `ring`. */
  override def variableBelief(vi: Int): Factor =
    Factor.multiply(problem.ring)(problem.domains)(problem.factorIdxOfVariable(vi).map(fi => messageValue(F2VMsg(fi,vi)))).normalize(problem.ring)

  def factorBelief(fi: Int): Factor = {
    val f = problem.factors(fi)
    Factor.multiply(problem.ring)(problem.domains)(problem.scopeOfFactor(fi).map(vi => messageValue(V2FMsg(vi,fi))) :+ f).normalize(problem.ring)
  }

  /** Throws if no clique contains `vars`.
    * @return Normalized belief over given variables in encoding specified by problem ring. */
  override def cliqueBelief(vars: Array[Var]): Factor = {
    val scope = vars.toSet
    val containingFactorIdx = problem.scopeOfFactor.zipWithIndex.find(fsc => scope.subsetOf(fsc._1.toSet)).get._2
    Factor.multiplyRetain(problem.ring)(problem.domains)(Seq(factorBelief(containingFactorIdx)),vars)
  }

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
      var fi = 0
      while (fi < problem.factors.length) {
        val fb: Array[Double] = factorBelief(fi).values
        factorEntropy = factorEntropy + problem.ring.entropy(fb)
        logExp = logExp + problem.ring.logExpectation(fb, problem.factors(fi).values)
        fi += 1
      }
    }

    val result = logExp + factorEntropy + variableEntropy
    result
  }

  /** @return Partition function in encoding specified by `ring`. */
  override def Z: Double = math.exp(logZ)
}

/** Stores just the raw message values of a BP run.
  * This class should not retain any references to other classes, and thus avoids memory leaks.
  */
class StoredResult(val problem: Problem, messages: BPResult#Message => Array[Double]) extends BPResult {

  val msgF2V: Array[Array[Array[Double]]] =
    problem.scopeOfFactor.zipWithIndex.map{case (scope,fi) => scope.map(v => new Array[Double](problem.domains(v)))}
  val msgV2F: Array[Array[Array[Double]]] =
    problem.scopeOfFactor.zipWithIndex.map{case (scope,fi) => scope.map(v => new Array[Double](problem.domains(v)))}

  {//copy stuff; this is not functional to avoid closing on `v2f` and `f2v`; and for performance
  var fi = 0
    while(fi < problem.numFactors){
      val scope = problem.scopeOfFactor(fi)
      var vii = 0
      while(vii < scope.length){
        val vi = scope(vii)

        msgF2V(fi)(vii) = messages(F2VMsg(fi, vi))
        msgV2F(fi)(vii) = messages(V2FMsg(vi, fi))

        vii += 1
      }
      fi += 1
    }
  }

  override def rawMessageValue(m: Message): Array[Double] = m match {
    case V2FMsg(vi,fi) => msgV2F(fi)(m.varIndexInFactorScope)
    case F2VMsg(fi,vi) => msgF2V(fi)(m.varIndexInFactorScope)
  }
}
