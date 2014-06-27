package vultura.fastfactors.inference.conditioned

import sun.jdbc.odbc.OdbcDef
import vultura.fastfactors.inference.cp2._
import vultura.fastfactors.{Problem, FastFactor}
import vultura.fastfactors.inference.MargParI
import vultura.util.SIIndex

/** Approximate BP solver plugin based on cp2 implementation. */
case class BPSolverPlugin(tol: Double = 1e-10, maxSteps: Long = 10000) extends ApproximateSolver[ExtendedBPResult]{
  override def name: String = s"BP[tol=$tol,maxSteps=$maxSteps]"
  override def result2mpi: <:<[ExtendedBPResult, MargParI] = implicitly[ExtendedBPResult <:< MargParI]

  //TODO make this incremental
  override def increment(oldState: ExtendedBPResult, oldProblem: Problem, newProblem: Problem): ExtendedBPResult = {
    val lbp: LBP = LBP(newProblem)
    val cal = new MutableFIFOCalibrator(lbp.cp)(MaxDiff, tol, maxSteps)
    createResult(lbp)(cal)
  }
  override def create(p: Problem): ExtendedBPResult = {
    val lbp: LBP = LBP(p)
    val cal = new MutableFIFOCalibrator(lbp.cp)(MaxDiff, tol, maxSteps)
    createResult(lbp)(cal)
  }

  def createResult(lbp: LBP)(cal: MutableFIFOCalibrator[lbp.BPMessage]) = {
    val p = lbp.problem
    val msgs: Map[Either[(Int, FastFactor), (FastFactor, Int)], (Array[Double], Long)] = (for{
      f <- p.factors
      v <- f.variables
      (m,e) <- Seq(Left((v,f)) -> lbp.V2F(v,f),Right((f,v)) -> lbp.F2V(f,v))
    } yield m -> (cal.edgeValue(e), cal.lastUpdateOf(e)))(collection.breakOut)
    CalResult(p,(f,i) => {
      val m = lbp.F2V(f,i)
      (cal.edgeValue(m),cal.lastUpdateOf(m))
    },(i,f) => {
      val m = lbp.V2F(i,f)
      (cal.edgeValue(m),cal.lastUpdateOf(m))
    },cal.iteration)
  }
}

case class CalResult(problem: Problem, f2v: (FastFactor,Int) => (Array[Double],Long), v2f: (Int,FastFactor) => (Array[Double],Long), iterations: Long) extends ExtendedBPResult with BPResult {
  val factorIndex: SIIndex[FastFactor] = new SIIndex(problem.factors.toSet)
  val msgF2V: Array[Array[Array[Double]]] = factorIndex.elements.map(f => f.variables.map(v => f2v(f,v)._1)(collection.breakOut): Array[Array[Double]])(collection.breakOut)
  val msgV2F: Array[Array[Array[Double]]] = factorIndex.elements.map(f => f.variables.map(v => v2f(v,f)._1)(collection.breakOut): Array[Array[Double]])(collection.breakOut)
  val updF2V: Array[Array[Long]] = factorIndex.elements.map(f => f.variables.map(v => f2v(f,v)._2)(collection.breakOut): Array[Long])(collection.breakOut)
  val updV2F: Array[Array[Long]] = factorIndex.elements.map(f => f.variables.map(v => v2f(v,f)._2)(collection.breakOut): Array[Long])(collection.breakOut)

  type Msg = Either[(Int,FastFactor),(FastFactor,Int)]
  def varOfMsg(m: Msg): Int = m.fold(_._1,_._2)

  def lastUpdate(edge: Msg): Long = edge match {
    case Left((v,f)) => updV2F(factorIndex(f))(f.variables.indexOf(v))
    case Right((f,v)) => updF2V(factorIndex(f))(f.variables.indexOf(v))
  }
  override def v2f(m: (Int, FastFactor)): FastFactor = FastFactor(Array(m._1),msgV2F(factorIndex(m._2))(m._2.variables.indexOf(m._1)))
  override def f2v(m: (FastFactor, Int)): FastFactor = FastFactor(Array(m._2),msgF2V(factorIndex(m._1))(m._1.variables.indexOf(m._2)))

  override def messageValue(m: Message): FastFactor = m match {
    case Left(vf) => v2f(vf)
    case Right(fv) => f2v(fv)
  }
}

trait ExtendedBPResult extends MargParI {
  type Message = Either[(Int,FastFactor),(FastFactor,Int)]
  def problem: Problem
  def messageValue(m: Message): FastFactor
  def lastUpdate(edge: Message): Long
  def iterations: Long
}
