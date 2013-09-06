package vultura.fastfactors.algorithms.gbp

import org.specs2._
import org.specs2.specification.Fragments
import vultura.fastfactors.Problem
import java.io.InputStreamReader

class RegionGraph$Test extends Specification {
  def is: Fragments =
    "load a file and print the dot of the bethe graph" ! {
      val problem = Problem.parseUAIProblem(new InputStreamReader(ClassLoader.getSystemResourceAsStream("uai/examples/grid2x2.uai"))).right.get
      val betheGraph = RegionGraph.betheRG(problem)
      println(betheGraph.toDot)
      true
    }
}
