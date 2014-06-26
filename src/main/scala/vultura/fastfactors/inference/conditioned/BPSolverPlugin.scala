package vultura.fastfactors.inference.conditioned

import vultura.fastfactors.inference.cp2.{BPResult, LBP, MaxDiff, MutableFIFOCalibrator}
import vultura.fastfactors.{Problem, FastFactor}
import vultura.fastfactors.inference.MargParI

/** Approximate BP solver plugin based on cp2 implementation. */
case class BPSolverPlugin(tol: Double = 1e-10, maxSteps: Long = 10000) extends ApproximateSolver[ExtendedBPResult]{
  override def name: String = s"BP[tol=$tol,maxSteps=$maxSteps]"
  override def result2mpi: <:<[ExtendedBPResult, MargParI] = implicitly[ExtendedBPResult <:< MargParI]

  //TODO make this incremental
  override def increment(oldState: ExtendedBPResult, oldProblem: Problem, newProblem: Problem): ExtendedBPResult =
    create(newProblem)

  override def create(p: Problem): ExtendedBPResult = {
    val lbp: LBP = LBP(p)
    val cal = new MutableFIFOCalibrator(lbp.cp)(MaxDiff, tol, maxSteps)
    new ExtendedBPResult with BPResult{
      type Msg = Either[(Int,FastFactor),(FastFactor,Int)]
      //interface methods
      def tuple2msg(t: Msg): lbp.BPMessage = t match {
        case Left((v,f))  => lbp.V2F(v,f)
        case Right((f,v)) => lbp.F2V(f,v)
      }
      def varOfMsg(m: Msg): Int = m.fold(_._1,_._2)

      override def messageValue(m: Message): FastFactor = FastFactor(Array(varOfMsg(m)),cal.edgeValue(tuple2msg(m)))
      override def lastUpdate(edge: Message): Long = cal.lastUpdateOf(tuple2msg(edge))
      override def iterations: Long = cal.iteration
      override def v2f(m: (Int, FastFactor)): FastFactor = messageValue(Left(m))
      override def f2v(m: (FastFactor, Int)): FastFactor = messageValue(Right(m))
      override def problem: Problem = p
    }
  }
}

trait ExtendedBPResult extends MargParI {
  type Message = Either[(Int,FastFactor),(FastFactor,Int)]
  def problem: Problem
  def messageValue(m: Message): FastFactor
  def lastUpdate(edge: Message): Long
  def iterations: Long
  def logZ: Double
}
