package vultura.factor.inference.conditioned.lcbp

import vultura.factor.inference.MargParI
import vultura.factor.inference.calibration.LBP
import vultura.util.graph.DotGraph

trait LCBPAlg {
  type R
  def infer(s: FactoredScheme, tol: Double = 1e-12, maxIter: Int = 10000): R
  def result(r: R): (MargParI, Boolean)
  def calibrationGraph(r: R): DotGraph[_]

  def inferWithScheme(s: FactoredScheme, tol: Double = 1e-12, maxIter: Int = 10000): (MargParI, Boolean) =
    result(infer(s,tol,maxIter))
  def graphFromScheme(s: FactoredScheme, tol: Double = 1e-12, maxIter: Int = 10000): DotGraph[_] =
    calibrationGraph(infer(s,tol,maxIter))
}

object LCBPAlg {
  val all = Seq(OldLCBP,LCBP_G_Exact,LCBP_G_BP,LCBP_BP)
}


/** The old implementation, using exact meta inference with counting. */
object OldLCBP extends LCBPAlg {
  override type R = LCBP

  override def infer(s: FactoredScheme, tol: Double, maxIter: Int): OldLCBP.R =
    new LCBP(s.problem,s.toGScheme,tol,maxIter)
  override def result(r: OldLCBP.R): (MargParI, Boolean) = (r,r.calibrator.isConverged)
  override def calibrationGraph(r: OldLCBP.R): DotGraph[_] = r.calibrator.toDot
}

/** The new lcbp implementation, using exact meta inference with junction tree. */
object LCBP_G_Exact extends LCBPAlg {
  override type R = LCBPGeneral

  override def infer(s: FactoredScheme, tol: Double, maxIter: Int): LCBP_G_Exact.R =
    new LCBPGeneral(s, maxUpdates = maxIter, tol = tol)
  override def result(r: LCBP_G_Exact.R): (MargParI, Boolean) = (r,r.calibrator.isConverged)
  override def calibrationGraph(r: LCBP_G_Exact.R): DotGraph[_] = r.calibrator.toDot
}

/** The new lcbp implementation, using bp as meta inference as plugin for general solver. */
object LCBP_G_BP extends LCBPAlg {
  override type R = LCBPGeneral

  override def infer(s: FactoredScheme, tol: Double, maxIter: Int): LCBP_G_BP.R = new LCBPGeneral(s, inferer = px => LBP.infer(px,maxIter,tol),maxUpdates = maxIter, tol = tol)
  override def result(r: LCBP_G_BP.R): (MargParI, Boolean) = (r,r.calibrator.isConverged)
  override def calibrationGraph(r: LCBP_G_BP.R): DotGraph[_] = r.calibrator.toDot
}

/** The native lcbp_bp implementation using a native bp implementation for meta inference. */
object LCBP_BP extends LCBPAlg {
  override type R = LcbpMetaBP

  override def infer(s: FactoredScheme, tol: Double, maxIter: Int): LCBP_BP.R = new LcbpMetaBP(s,maxIter,tol)
  override def result(r: LCBP_BP.R): (MargParI, Boolean) = (r,r.calibrator.isConverged)
  override def calibrationGraph(r: LCBP_BP.R): DotGraph[_] = r.calibrator.toDot
}