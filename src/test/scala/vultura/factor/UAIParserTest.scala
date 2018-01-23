package vultura.factor

import java.io.StringReader

import org.specs2.mutable.Specification

/**
 * Test parsing of *.uai files into [[vultura.factor.Problem]].
 */
class UAIParserTest extends Specification with FactorMatchers {

  "parse simple problem from string" in {
    val uaiString = "MARKOV 1 2 1 1 1 2 0.5 0.5"
    Problem.parseUAIProblem(new StringReader(uaiString)) must beRight
  }

  "parse all example uai files" in {
    foreach(SampleProblems.examples) {exp =>
      exp.loadedProblem must beRight
    }
  }

  "serialize/deserialize 2x2 grid with |D| = 3" should {
    val p: Problem = vultura.factor.generators.grid(2,2,3)
    forall(Problem.fromUaiString(p.uaiString).factors.zip(p.factors)){
      case (fr,original) => fr must beSimilarTo(original,1e-9)
    }
  }
}
