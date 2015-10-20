package vultura.factor.inference.gbp

import org.specs2._
import vultura.factor.Problem
import scala.util.Random

/**
 * Created by IntelliJ IDEA.
 * User: Thomas Geier
 * Date: 9/5/13
 */
class RegionGraphTest extends Specification {
  def loadProblem(path: String) = Problem.parseUAIProblem(Thread.currentThread().getContextClassLoader.getResourceAsStream(path)).right.get
  val familyProblem = loadProblem("problems/uai/examples/Family2Dominant.1.5loci.uai")
  val grid4 = loadProblem("problems/uai/examples/grid4x4.uai")

  def is = s2""""""
}
