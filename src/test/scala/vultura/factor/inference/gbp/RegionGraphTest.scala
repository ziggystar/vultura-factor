package vultura.factor.inference.gbp

import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragments
import vultura.factor.{SampleProblems, Problem}


class RegionGraphTest extends Specification {
  def loadProblem(path: String) = Problem.parseUAIProblem(Thread.currentThread().getContextClassLoader.getResourceAsStream(path)).right.get
  val familyProblem = loadProblem("problems/uai/examples/Family2Dominant.1.5loci.uai")
  val grid4 = loadProblem("problems/uai/examples/grid4x4.uai")

  "test self-checks on RegionGraph" >> {
    "bethe rg must pass all checks for all example problems" >>
      Fragments(
        SampleProblems.examples.map{ case example =>
          example.filename >> (RegionGraph.betheRG(example.problem).Diagnosis.validityIssues must beEmpty)
        }:_*
      )
  }
}
