package vultura.factor.inference.conditioned.lcbp

import vultura.factor.Problem
import vultura.factor.inference.MargParI
import vultura.factor.inference.calibration.LBP

trait LCBPAlg[X] {
  def inferWithScheme(s: FactoredScheme, tol: Double = 1e-12, maxIter: Int = 10000): (MargParI, Boolean, X)
}

/** The old implementation, using exact meta inference with counting. */
object OldLCBP extends LCBPAlg[LCBP] {
  override def inferWithScheme(s: FactoredScheme, tol: Double = 1e-12, maxIter: Int = 10000): (MargParI, Boolean, LCBP) = {
    val lcbp = new LCBP(s.problem,s.toGScheme,tol,maxIter)
    (lcbp,lcbp.calibrator.isConverged,lcbp)
  }
}

/** The new lcbp implementation, using exact meta inference with junction tree. */
object LCBP_G_Exact extends LCBPAlg[LCBPGeneral] {
  override def inferWithScheme(s: FactoredScheme, tol: Double, maxIter: Int): (MargParI, Boolean, LCBPGeneral) = {
    val lcbp = new LCBPGeneral(s, maxUpdates = maxIter, tol = tol)
    (lcbp,lcbp.calibrator.isConverged,lcbp)
  }
}

/** The new lcbp implementation, using bp as meta inference as plugin for general solver. */
object LCBP_G_BP extends LCBPAlg[LCBPGeneral] {
  override def inferWithScheme(s: FactoredScheme, tol: Double, maxIter: Int): (MargParI, Boolean, LCBPGeneral) = {
    val lcbp = new LCBPGeneral(s, inferer = px => LBP.infer(px,maxIter,tol),maxUpdates = maxIter, tol = tol)
    (lcbp,lcbp.calibrator.isConverged,lcbp)
  }
}

/** The native lcbp_bp implementation using a native bp implementation for meta inference. */
object LCBP_BP extends LCBPAlg[LcbpMetaBP] {
  override def inferWithScheme(s: FactoredScheme, tol: Double, maxIter: Int): (MargParI, Boolean, LcbpMetaBP) = {
    val lcbp = new LcbpMetaBP(s,maxIter,tol)
    (lcbp,lcbp.calibrator.isConverged,lcbp)
  }
}