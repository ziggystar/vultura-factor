package vultura.factor

import org.specs2._

/**
 * Test parsing of *.uai files into [[vultura.factor.Problem]].
 */
class UAIParserTest extends Specification {
  def is =
    "parse all example uai files" ! (SampleProblems.examples must contain((p: SampleProblems.Example) => {
      f"${p.filename} is ok" ==> (p.problem must not (throwAn[Exception]))
    }).forall)
}
