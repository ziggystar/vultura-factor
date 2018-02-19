package vultura.factor.inference

import org.specs2.Specification
import vultura.factor.inference.conditioned.{CBP, CBPConfig}
import vultura.factor.{Problem, generators}

import scala.util.Random

/**
 * @author Thomas Geier
 * @since 6/23/13
 */

class CBPTest extends Specification {
  val randomProblem = generators.grid(6,6,2,generators.expGauss(1),new Random(0))
  def inferCBP(p: Problem, steps: Int = 10, seed: Long = 1) = CBPConfig().iterator(p,seed).drop(steps).next()
  def testDeterminsim(config: CBPConfig) =
    f"config $config" ! (config.iterator(randomProblem,1).drop(3).next().toResult === config.iterator(randomProblem,1).drop(3).next().toResult) ^
      f"config $config" ! (config.iterator(randomProblem,2).drop(3).next().toResult !== config.iterator(randomProblem,1).drop(3).next().toResult)

  def is =
  "deterministic behaviour" ^
    testDeterminsim(CBPConfig()) ^
    testDeterminsim(CBPConfig(variableSelection = CBP.VARIABLE_SELECTION.LAST_UPDATE)) ^
  "entropy of conditions" ! (inferCBP(randomProblem).conditionEntropy must beLessThan[Double](10))
}