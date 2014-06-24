package vultura.fastfactors.inference.cp2

import org.specs2._
import org.specs2.specification.Fragments
import vultura.fastfactors.generators._
import vultura.fastfactors.inference.BeliefPropagation

/**
 * Created by thomas on 19.05.14.
 */
class LBPTest extends Specification {
  val small = grid(2,3,2,expGauss(0.2))
  val lbp_small = LBP(small)
  def c_small = new MutableFIFOCalibrator[lbp_small.BPMessage](MaxDiff, maxSteps = 10000, problem = lbp_small.cp, tol = 1e-4)
  def increment_small = new MutableFIFOCalibrator[lbp_small.BPMessage](MaxDiff, problem = lbp_small.cp, initialize = c_small, tol = 1e-4)
  val p1 = grid(5,5,2,expGauss(0.5))
  val lbp: LBP = LBP(p1)
  def c_p1 = new MutableFIFOCalibrator[lbp.BPMessage](MaxDiff, maxSteps = 10000, problem = lbp.cp, tol = 1e-10)
  def increment1_p1 = new MutableFIFOCalibrator[lbp.BPMessage](MaxDiff, problem = lbp.cp, initialize = c_p1, tol = 1e-10)
  val tree = treeK(20,3,2,expGauss(1))

  override def is: Fragments = {
    "bp converged on p1 after some steps" ! (c_p1.isConverged and (c_p1.iteration !== 0)) ^
    "reusing old messages needs no updates" ! (increment1_p1.iteration === 0) ^
    "reusing old messages needs no updates small problem" ! (increment_small.iteration === 0) ^
    "cp2.LBP must infer correct result on tree" ! (LBP.infer(tree).logZ must beCloseTo(tree.logZ,1e-12)) ^
    "cp2.LBP must infer same logZ as old LBP" ! (LBP.infer(p1).logZ must beCloseTo(new BeliefPropagation(p1, tol = 1e-10, runInitially = 10000).logZ, 1e-7))
  }
}
