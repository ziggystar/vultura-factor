package vultura.factor.inference.gbp

import org.specs2._
import org.specs2.specification.Fragments
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

  def is: Fragments =
    "setClosure method" ! (RegionGraph.setClosure(Set(3,7))(xs => for(x <- xs;y <- xs; z = x - y if z > 0) yield z) === Set(3,7,4,1,6,2,5)) ^
    "saturated region graph construction" ^
      "grid 4x4" ! (RegionGraph.saturatedRG(grid4,new Random(0)).issues === Seq()) ^
      "family problem" ! (RegionGraph.saturatedRG(familyProblem,new Random(0)).issues === Seq())
}
