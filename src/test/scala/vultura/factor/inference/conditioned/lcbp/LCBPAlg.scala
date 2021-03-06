package vultura.factor.inference.conditioned.lcbp

import vultura.factor.inference.MargParI
import vultura.factor.inference.calibration.LBP
import vultura.util.graph.DotGraph

trait LCBPAlg { self: Product =>
  type R
  def infer(s: FactoredScheme, tol: Double = 1e-12, maxIter: Int = 10000): R
  def result(r: R): (MargParI, Boolean)
  def calibrationGraph(r: R): DotGraph[_]

  def inferWithScheme(s: FactoredScheme, tol: Double = 1e-12, maxIter: Int = 10000): (MargParI, Boolean) =
    result(infer(s,tol,maxIter))
  def graphFromScheme(s: FactoredScheme, tol: Double = 1e-12, maxIter: Int = 10000): DotGraph[_] =
    calibrationGraph(infer(s,tol,maxIter))
  def nodeCSV(r: R): String

  override def toString: String = productPrefix
}

object LCBPAlg {
  val all = Seq(
    OldLCBP,
    LCBP_G_Exact,
    LCBP_G_BP,
    LCBP_BP(corrected = false),
    LCBP_BP(corrected = true),
    LCBP_G_F_Exact(false),
    LCBP_G_F_Exact(true),
    LCBP_G_F_BP(false),
    LCBP_G_F_BP(true)
  )
}


/** The old implementation, using exact meta inference with counting. */
case object OldLCBP extends LCBPAlg {
  override type R = LCBP

  override def infer(s: FactoredScheme, tol: Double, maxIter: Int): OldLCBP.R =
    new LCBP(s.problem,s.toGScheme,tol,maxIter)
  override def result(r: OldLCBP.R): (MargParI, Boolean) = (r,r.calibrator.isConverged)
  override def calibrationGraph(r: OldLCBP.R): DotGraph[_] = r.calibrator.toDot

  override def nodeCSV(r: R): String = r.calibrator.toCSV
}

/** The new lcbp implementation, using exact meta inference with junction tree. */
case object LCBP_G_Exact extends LCBPAlg {
  override type R = LCBPGeneral

  override def infer(s: FactoredScheme, tol: Double, maxIter: Int): LCBP_G_Exact.R =
    new LCBPGeneral(s, maxUpdates = maxIter, tol = tol)
  override def result(r: LCBP_G_Exact.R): (MargParI, Boolean) = (r,r.calibrator.isConverged)
  override def calibrationGraph(r: LCBP_G_Exact.R): DotGraph[_] = r.calibrator.toDot
  override def nodeCSV(r: R): String = r.calibrator.toCSV
}

/** The new lcbp implementation, using bp as meta inference as plugin for general solver. */
case object LCBP_G_BP extends LCBPAlg {
  override type R = LCBPGeneral

  override def infer(s: FactoredScheme, tol: Double, maxIter: Int): LCBP_G_BP.R = new LCBPGeneral(s, inferer = px => LBP.infer(px,maxIter,tol),maxUpdates = maxIter, tol = tol)
  override def result(r: LCBP_G_BP.R): (MargParI, Boolean) = (r,r.calibrator.isConverged)
  override def calibrationGraph(r: LCBP_G_BP.R): DotGraph[_] = r.calibrator.toDot
  override def nodeCSV(r: R): String = r.calibrator.toCSV
}

/** The native lcbp_bp implementation using a native bp implementation for meta inference. */
case class LCBP_BP(corrected: Boolean) extends LCBPAlg {
  override type R = LcbpMetaBP

  override def infer(s: FactoredScheme, tol: Double, maxIter: Int): R =
    new LcbpMetaBP(s,maxIter,tol,useDeltaTerm = corrected)
  override def result(r: R): (MargParI, Boolean) = (r,r.calibrator.isConverged)
  override def calibrationGraph(r: R): DotGraph[_] = r.calibrator.toDot
  override def nodeCSV(r: R): String = r.calibrator.toCSV

  override def toString: String = "LCBP_BP" + (if(corrected) "+Cor" else "")
}

/** The new lcbp implementation, using exact meta inference with junction tree. */
case class LCBP_G_F_Exact(corrected: Boolean) extends LCBPAlg {
  override type R = LCBPFactoredGeneral

  override def infer(s: FactoredScheme, tol: Double, maxIter: Int): R =
    new LCBPFactoredGeneral(s, maxUpdates = maxIter, tol = tol, useDeltaTerm = corrected)
  override def result(r: R): (MargParI, Boolean) = (r,r.calibrator.isConverged)
  override def calibrationGraph(r: R): DotGraph[_] = r.calibrator.toDot
  override def nodeCSV(r: R): String = r.calibrator.toCSV
  override def toString: String = "LCBP_G_F_Exact" + (if(corrected) "+Cor" else "")
}

/** The new lcbp implementation, using bp as meta inference as plugin for general solver. */
case class LCBP_G_F_BP(corrected: Boolean) extends LCBPAlg {
  override type R = LCBPFactoredGeneral

  override def infer(s: FactoredScheme, tol: Double, maxIter: Int): R =
    new LCBPFactoredGeneral(s, inferer = px => LBP.infer(px,maxIter,tol),maxUpdates = maxIter, tol = tol, useDeltaTerm = corrected)
  override def result(r: R): (MargParI, Boolean) = (r,r.calibrator.isConverged)
  override def calibrationGraph(r: R): DotGraph[_] = r.calibrator.toDot
  override def nodeCSV(r: R): String = r.calibrator.toCSV
  override def toString: String = "LCBP_G_F_BP" + (if(corrected) "+Cor" else "")
}