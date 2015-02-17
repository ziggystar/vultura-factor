package vultura.factor.inference.conditioned

import vultura.factor.inference.MargParI
import vultura.factor.inference.calibration._
import vultura.factor.Problem

/** Approximate BP solver plugin based on cp2 implementation. */
case class BPSolverPlugin(tol: Double = 1e-10, maxSteps: Long = 10000) extends ApproximateSolver[ExtendedBPResult]{
  override def name: String = s"BP[tol=$tol,maxSteps=$maxSteps]"
  override def result2mpi: <:<[ExtendedBPResult, MargParI] = implicitly[ExtendedBPResult <:< MargParI]

  //TODO make this incremental
  override def increment(oldState: ExtendedBPResult, newProblem: Problem): ExtendedBPResult = {
    val lbp: LBP = LBP(newProblem)
    val cal = new MutableFIFOCalibrator(lbp.edges)(ConvergenceTest.MaxDiff(tol), maxSteps, new EdgeValues[lbp.BPMessage] {
      override def hasEdge(e: lbp.BPMessage): Boolean = oldState.problem.scopeOfFactor(e.fi).contains(e.v)
      override def edgeValue(e: lbp.BPMessage): e.type#TOut = e match {
        case lbp.V2F(vi,fi) => oldState.rawMessageValue(oldState.V2FMsg(vi,fi))
        case lbp.F2V(fi,vi) => oldState.rawMessageValue(oldState.F2VMsg(fi,vi))
      }
    } orElse lbp.maxEntInitializer)
    createResult(lbp)(cal)
  }
  override def create(p: Problem): ExtendedBPResult = {
    val lbp: LBP = LBP(p)
    val cal = new MutableFIFOCalibrator(lbp.edges)(ConvergenceTest.MaxDiff(tol), maxSteps, lbp.maxEntInitializer)
    createResult(lbp)(cal)
  }

  def createResult(lbp: LBP)(cal: MutableFIFOCalibrator[lbp.BPMessage]) = {
    val p = lbp.problem

    def convMsg(m: BPResult#Message): lbp.BPMessage = {
      m match {
        case _:BPResult#V2FMsg => lbp.V2F(m.vi,m.fi)
        case _:BPResult#F2VMsg => lbp.F2V(m.fi,m.vi)
      }
    }
    new CalResult(p,m => (cal.edgeValue(convMsg(m)),cal.lastUpdateOf(convMsg(m))),cal.iteration)
  }
}

class CalResult(val problem: Problem, messages: BPResult#Message => (Array[Double],Long), val iterations: Long) extends ExtendedBPResult {

  val msgF2V: Array[Array[Array[Double]]] =
    problem.scopeOfFactor.zipWithIndex.map{case (scope,fi) => scope.map(v => new Array[Double](problem.domains(v)))}
  val msgV2F: Array[Array[Array[Double]]] =
    problem.scopeOfFactor.zipWithIndex.map{case (scope,fi) => scope.map(v => new Array[Double](problem.domains(v)))}
  val updF2V: Array[Array[Long]] = problem.scopeOfFactor.map(sc => new Array[Long](sc.length))
  val updV2F: Array[Array[Long]] = problem.scopeOfFactor.map(sc => new Array[Long](sc.length))

  {//copy stuff; this is not functional to avoid closing on `v2f` and `f2v`; and for performance
    var fi = 0
    while(fi < problem.numFactors){
      val scope = problem.scopeOfFactor(fi)
      var vii = 0
      while(vii < scope.length){
        val vi = scope(vii)

        {
          val (f2v_r, f2v_i) = messages(F2VMsg(fi,vi))
          msgF2V(fi)(vii) = f2v_r
          updF2V(fi)(vii) = f2v_i
        }

        {
          val (v2f_r, v2f_i) = messages(V2FMsg(vi,fi))
          msgV2F(fi)(vii) = v2f_r
          updV2F(fi)(vii) = v2f_i
        }

        vii += 1
      }
      fi += 1
    }
  }

  def lastUpdate(edge: Message): Long = edge match {
    case V2FMsg(vi,fi) => updV2F(fi)(edge.varIndexInFactorScope)
    case F2VMsg(fi,vi) => updF2V(fi)(edge.varIndexInFactorScope)
  }


  override def rawMessageValue(m: Message): Array[Double] = m match {
    case V2FMsg(vi,fi) => msgV2F(fi)(m.varIndexInFactorScope)
    case F2VMsg(fi,vi) => msgF2V(fi)(m.varIndexInFactorScope)
  }
}

trait ExtendedBPResult extends BPResult {
  def lastUpdate(edge: Message): Long
  def iterations: Long
}
