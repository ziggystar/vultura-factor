package vultura.fastfactors.inference.cp2

import org.specs2._
import org.specs2.specification.Fragments
import vultura.fastfactors.generators._

/**
 * Created by thomas on 19.05.14.
 */
class LBPTest extends Specification {
  val p1 = grid(5,5,2,expGauss(0.5))
  override def is: Fragments = {
    "test on random grid" ! {
      val lbp: LBP = LBP(p1)
      val c = new MutableFIFOCalibrator[lbp.BPMessage](MaxDiff,maxSteps = 10000)(lbp.cp)
      println(c.iteration)
      true
    }

  }
}
