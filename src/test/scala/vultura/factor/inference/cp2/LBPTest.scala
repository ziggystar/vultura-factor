package vultura.factor.inference.cp2

import org.specs2._
import org.specs2.specification.Fragments
import vultura.factor.generators._
import vultura.factor.inference.BeliefPropagation
import vultura.factor.inference.calibration.{LBP, MutableFIFOCalibrator, MaxDiff}

class LBPTest extends Specification {
  val small = grid(2,3,2,expGauss(0.2))
  val lbp_small = LBP(small)
  def c_small = new MutableFIFOCalibrator[lbp_small.BPMessage](lbp_small.cp)(MaxDiff, maxSteps = 10000, tol = 1e-4)
  def increment_small = new MutableFIFOCalibrator[lbp_small.BPMessage](lbp_small.cp)(MaxDiff, initialize = c_small, tol = 1e-4)
  val p1 = grid(5,5,2,expGauss(0.5))
  val lbp: LBP = LBP(p1)
  def c_p1 = new MutableFIFOCalibrator[lbp.BPMessage](lbp.cp)(MaxDiff, maxSteps = 10000, tol = 1e-10)
  def increment1_p1 = new MutableFIFOCalibrator[lbp.BPMessage](lbp.cp)(MaxDiff, initialize = c_p1, tol = 1e-10)
  val tree = treeK(20,3,2,expGauss(1))

  override def is: Fragments = {
    "bp converged on p1 after some steps" ! (c_p1.isConverged and (c_p1.iteration !== 0)) ^
    "reusing old messages needs no updates" ! (increment1_p1.iteration === 0) ^
    "reusing old messages needs no updates small problem" ! (increment_small.iteration === 0) ^
    "cp2.LBP must infer correct result on tree" ! (LBP.infer(tree).logZ must beCloseTo(tree.logZ,1e-12)) ^
    "cp2.LBP must infer same logZ as old LBP" ! (LBP.infer(p1).logZ must beCloseTo(new BeliefPropagation(p1, tol = 1e-10, runInitially = 1000).logZ, 1e-7))
  }
}
