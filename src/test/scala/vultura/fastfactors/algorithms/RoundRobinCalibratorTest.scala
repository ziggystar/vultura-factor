package vultura.fastfactors.algorithms

import vultura.fastfactors.generators._
import scala.util.Random
import org.specs2._
import org.specs2.specification.Fragments

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 11/8/13
 */
class RoundRobinCalibratorTest extends Specification {
  val maxEntProblem = grid(3,3,2,maxEntropy, new Random(0))
  val maxEntCalibrated = new RoundRobinCalibrator(CalibrationProblem.betheCalibrationProblem(maxEntProblem))

  import CalibrationProblem.{VariableN => Var}
  def is: Fragments =
  {
    println(CalibrationProblem.betheCalibrationProblem(maxEntProblem).toDot)
//    println(maxEntCalibrated.resultOf(Var(0)).values.deep)
    true
  }

}
