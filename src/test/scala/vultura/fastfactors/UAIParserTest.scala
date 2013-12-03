package vultura.fastfactors

import org.specs2._
import org.specs2.specification.Fragments

/**
 * Test parsing of *.uai files into fastfactor.Problem.
 */
class UAIParserTest extends Specification {
  def is: Fragments =
    "parse all example uai files" ! (SampleProblems.examples must contain((p: SampleProblems.Example) => {
      f"${p.filename} is ok" ==> (p.problem must not (throwAn[Exception]))
    }).forall)
}
