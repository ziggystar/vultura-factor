package vultura.fastfactors.generators

import com.google.caliper.Benchmark
import scala.util.Random
import vultura.fastfactors.algorithms.CalibratedJunctionTree
import com.google.caliper.runner.CaliperMain

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class FastFactorBenchmark  extends Benchmark {
  val smallGridProblem = grid(4,4,2,expGauss(),new Random(1))

  def smallJunctionTree(repeats: Int): Unit = {
    var i = 0
    while(i < repeats){
      new CalibratedJunctionTree(smallGridProblem)
      i = i - 1
    }
  }
}

object Test extends App {
  CaliperMain.main(classOf[FastFactorBenchmark],args)
}
