package vultura.factor.inference.conditioned

import vultura.factor.inference.MargParI
import vultura.factor.inference.cp2._
import vultura.factor.{Factor, Problem}
import vultura.util.SIIndex

/** Approximate BP solver plugin based on cp2 implementation. */
case class BPSolverPlugin(tol: Double = 1e-10, maxSteps: Long = 10000) extends ApproximateSolver[ExtendedBPResult]{
  override def name: String = s"BP[tol=$tol,maxSteps=$maxSteps]"
  override def result2mpi: <:<[ExtendedBPResult, MargParI] = implicitly[ExtendedBPResult <:< MargParI]

  //TODO make this incremental
  override def increment(oldState: ExtendedBPResult, newProblem: Problem): ExtendedBPResult = {
    val lbp: LBP = LBP(newProblem)
    val cal = new MutableFIFOCalibrator(lbp.cp)(MaxDiff, tol, maxSteps, new EdgeValues[lbp.BPMessage] {
      override def hasEdge(e: lbp.BPMessage): Boolean = oldState.problem.factorsOfVariable(e.v).contains(e.f)
      override def edgeValue(e: lbp.BPMessage): e.type#TOut = e match {
        case lbp.V2F(v,f) => oldState.messageValue(Left((v,f))).values
        case lbp.F2V(f,v) => oldState.messageValue(Right((f,v))).values
      }
    })
    createResult(lbp)(cal)
  }
  override def create(p: Problem): ExtendedBPResult = {
    val lbp: LBP = LBP(p)
    val cal = new MutableFIFOCalibrator(lbp.cp)(MaxDiff, tol, maxSteps)
    createResult(lbp)(cal)
  }

  def createResult(lbp: LBP)(cal: MutableFIFOCalibrator[lbp.BPMessage]) = {
    val p = lbp.problem
    val msgs: Map[Either[(Int, Factor), (Factor, Int)], (Array[Double], Long)] = (for{
      f <- p.factors
      v <- f.variables
      (m,e) <- Seq(Left((v,f)) -> lbp.V2F(v,f),Right((f,v)) -> lbp.F2V(f,v))
    } yield m -> (cal.edgeValue(e), cal.lastUpdateOf(e)))(collection.breakOut)
    new CalResult(p,(f,i) => {
      val m = lbp.F2V(f,i)
      (cal.edgeValue(m),cal.lastUpdateOf(m))
    },(i,f) => {
      val m = lbp.V2F(i,f)
      (cal.edgeValue(m),cal.lastUpdateOf(m))
    },cal.iteration)
  }
}

class CalResult(val problem: Problem, f2v: (Factor,Int) => (Array[Double],Long), v2f: (Int,Factor) => (Array[Double],Long), val iterations: Long) extends ExtendedBPResult with BPResult {
  val factorIndex: SIIndex[Factor] = new SIIndex(problem.factors.toSet)
  val msgF2V: Array[Array[Array[Double]]] = factorIndex.elements.map(f => f.variables.map(v => new Array[Double](problem.domains(v)))(collection.breakOut): Array[Array[Double]])(collection.breakOut)
  val msgV2F: Array[Array[Array[Double]]] = factorIndex.elements.map(f => f.variables.map(v => new Array[Double](problem.domains(v)))(collection.breakOut): Array[Array[Double]])(collection.breakOut)
  val updF2V: Array[Array[Long]] = factorIndex.elements.map(f => new Array[Long](f.variables.size))(collection.breakOut)
  val updV2F: Array[Array[Long]] = factorIndex.elements.map(f => new Array[Long](f.variables.size))(collection.breakOut)

  {//copy stuff; this is not functional to avoid closing on `v2f` and `f2v`; and for performance
    var fi = 0
    while(fi < factorIndex.size){
      val f = factorIndex.backward(fi)
      var vi = 0
      while(vi < f.variables.size){
        val v = f.variables(vi)

        {
          val (f2v_r, f2v_i) = f2v(f, v)
          msgF2V(fi)(vi) = f2v_r
          updF2V(fi)(vi) = f2v_i
        }

        {
          val (v2f_r, v2f_i) = v2f(v, f)
          msgV2F(fi)(vi) = v2f_r
          updV2F(fi)(vi) = v2f_i
        }

        vi = vi + 1
      }
      fi = fi + 1
    }
  }

  type Msg = Either[(Int,Factor),(Factor,Int)]
  def varOfMsg(m: Msg): Int = m.fold(_._1,_._2)

  def lastUpdate(edge: Msg): Long = edge match {
    case Left((v,f)) => updV2F(factorIndex(f))(f.variables.indexOf(v))
    case Right((f,v)) => updF2V(factorIndex(f))(f.variables.indexOf(v))
  }
  override def v2f(m: (Int, Factor)): Factor = Factor(Array(m._1),msgV2F(factorIndex(m._2))(m._2.variables.indexOf(m._1)))
  override def f2v(m: (Factor, Int)): Factor = Factor(Array(m._2),msgF2V(factorIndex(m._1))(m._1.variables.indexOf(m._2)))

  override def messageValue(m: Message): Factor = m match {
    case Left(vf) => v2f(vf)
    case Right(fv) => f2v(fv)
  }
}

trait ExtendedBPResult extends MargParI {
  type Message = Either[(Int,Factor),(Factor,Int)]
  def problem: Problem
  def messageValue(m: Message): Factor
  def lastUpdate(edge: Message): Long
  def iterations: Long
}
