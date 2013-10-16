package vultura.fastfactors

import org.specs2._
import org.specs2.specification.Fragments

/**
 * Test parsing of *.uai files into fastfactor.Problem.
 */
class UAIParserTest extends Specification {
  val exampleProblems = """uai/examples/30markers.uai
                          |uai/examples/grid4x4.uai
                          |uai/examples/deer_rescaled_0034.K10.F1.25.model.uai
                          |uai/examples/or_chain_10.fg.uai
                          |uai/examples/grid_tg.uai
                          |uai/examples/network.uai
                          |uai/examples/smokers_10.uai
                          |uai/examples/GEOM30a_3.uai
                          |uai/examples/grid3x3.uai
                          |uai/examples/10_14_s.binary.uai
                          |uai/examples/Family2Dominant.1.5loci.uai
                          |uai/examples/grid2x2.uai
                          |uai/examples/grid10x10.f2.wrap.uai""".stripMargin.split("\n").toSeq
  def is: Fragments =
    "parse all example uai files" ! (exampleProblems must contain((p: String) => {
      f"$p is ok" ==> ({Problem.parseUAIProblem(ClassLoader.getSystemResourceAsStream(p)); p} must not (throwAn[Exception]))
    }).forall)
}
