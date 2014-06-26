package vultura.fastfactors.inference.cp2

import org.specs2._
import org.specs2.specification.Fragments
import vultura.fastfactors.generators._

/**
 * Created by thomas on 19.05.14.
 */
class LBPTest extends Specification {
  val p1 = grid(5,5,2,expGauss(0.5))
  val lbp: LBP = LBP(p1)
  val c_p1 = new MutableFIFOCalibrator[lbp.BPMessage](MaxDiff, maxSteps = 10000, problem = lbp.cp)
  val increment1_p1 = new MutableFIFOCalibrator[lbp.BPMessage](MaxDiff, problem = lbp.cp, initialize = c_p1)
  val tree = treeK(20,3,2,expGauss(1))

  override def is: Fragments = {
    "bp converged on p1 after some steps" ! (c_p1.isConverged and (c_p1.iteration !== 0)) ^
    "reusing old messages needs no updates" ! (increment1_p1.iteration === 0) ^
    "cp2.LBP must infer correct result on tree" ! (LBP.infer(tree).logZ must beCloseTo(tree.logZ,1e-12))
  }
}
