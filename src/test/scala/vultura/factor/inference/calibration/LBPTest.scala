package vultura.factor.inference.calibration

import org.specs2._
import org.specs2.specification.Fragments
import vultura.factor.generators._
import vultura.factor.inference.BeliefPropagation

class LBPTest extends Specification {
  val small = grid(2,3,2,expGauss(0.2))
  val lbp_small = LBP(small)
  def c_small = new MutableFIFOCalibrator[lbp_small.BPMessage](lbp_small.edges)(ConvergenceTest.MaxDiff(1e-4), maxSteps = 10000, lbp_small.maxEntInitializer)
  def increment_small = new MutableFIFOCalibrator[lbp_small.BPMessage](lbp_small.edges)(ConvergenceTest.MaxDiff(1e-4), initialize = c_small)
  val p1 = grid(5,5,2,expGauss(0.5))
  val lbp: LBP = LBP(p1)
  def c_p1 = new MutableFIFOCalibrator[lbp.BPMessage](lbp.edges)(ConvergenceTest.MaxDiff(1e-10), maxSteps = 10000, lbp.maxEntInitializer)
  def increment1_p1 = new MutableFIFOCalibrator[lbp.BPMessage](lbp.edges)(ConvergenceTest.MaxDiff(1e-10), initialize = c_p1)
  val tree = treeK(20,3,2,expGauss(1))

  override def is: Fragments = {
    "bp converged on p1 after some steps" ! (c_p1.isConverged and (c_p1.iteration !== 0)) ^
    "reusing old messages needs no updates" ! (increment1_p1.iteration === 0) ^
    "reusing old messages needs no updates small problem" ! (increment_small.iteration === 0) ^
    "cp2.LBP must infer correct result on tree" ! (LBP.infer(tree).logZ must beCloseTo(tree.logZ,1e-12)) ^
    "cp2.LBP must infer same logZ as old LBP" ! (LBP.infer(p1).logZ must beCloseTo(new BeliefPropagation(p1, tol = 1e-10, runInitially = 1000).logZ, 1e-7))
  }
}
