package vultura.fastfactors.algorithms

import vultura.fastfactors.generators._
import scala.util.Random
import org.specs2._
import org.specs2.specification.Fragments
import vultura.fastfactors.{FastFactorSpecs, Problem}

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 11/8/13
 */
class RoundRobinCalibratorTest extends Specification with FastFactorSpecs {
  val maxEntProblem = grid(2,2,2,maxEntropy, new Random(0))
  val randomGrid = grid(3,3,2,expGauss(1),new Random(0))
  val maxEntCalibrated = new RoundRobinCalibrator(CalibrationProblem.betheCalibrationProblem(maxEntProblem))

  import CalibrationProblem.{VariableN => Var}
  def is: Fragments =
    "calibrated Bethe graph should have same marginals as BP" ^
      "on maxent problem" ! compareCPwithBP(maxEntProblem) ^
      "on random grid" ! compareCPwithBP(randomGrid)

  def compareCPwithBP(p: Problem) = {
    val calibrator = new RoundRobinCalibrator(CalibrationProblem.betheCalibrationProblem(p))
    val bpResult = new BeliefPropagation(p,new Random(0),1e-9,1000)
    p.variables.map{v =>
      calibrator.resultOf(Var(v)) must haveValuesCloseTo(bpResult.variableBelief(v),0.001)
    }.reduce(_ and _)
  }
}
