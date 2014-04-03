package vultura.fastfactors.inference

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.fastfactors.{Problem, generators}
import scala.util.Random
import vultura.fastfactors.inference.conditioned.{CBPConfig, CBP}

/**
 * @author Thomas Geier
 * @since 6/23/13
 */

class CBPTest extends Specification {
  val randomProblem = generators.grid(6,6,2,generators.expGauss(1),new Random(0))
  def inferCBP(p: Problem, steps: Int = 10, seed: Long = 1) = CBPConfig().iterator(p,seed).drop(steps).next()
  def testDeterminsim(config: CBPConfig): Fragments =
    f"config $config" ! (config.iterator(randomProblem,1).drop(3).next().toResult === config.iterator(randomProblem,1).drop(3).next().toResult) ^
      f"config $config" ! (config.iterator(randomProblem,2).drop(3).next().toResult !== config.iterator(randomProblem,1).drop(3).next().toResult)

  def is: Fragments =
  "deterministic behaviour" ^
    testDeterminsim(CBPConfig()) ^
    testDeterminsim(CBPConfig(variableSelection = CBP.VARIABLE_SELECTION.LAST_UPDATE)) ^
  "entropy of conditions" ! (inferCBP(randomProblem).conditionEntropy must beLessThan[Double](10))
}