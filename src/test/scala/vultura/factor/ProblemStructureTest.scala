package vultura.factor

import org.scalacheck.Prop
import org.specs2.Specification
import org.specs2.matcher.ScalaCheckMatchers
import org.specs2.specification.Fragments
import vultura.factor.generators._

class ProblemStructureTest extends Specification with SCProblemGen with ScalaCheckMatchers {

  override def is: Fragments = {
    "neighbours on 2x2 grid" ^
      "exclusive" ! (grid(2,2).neighboursOfVariableEx(0).toSet === Set(1,2)) ^
      "inclusive" ! (grid(2,2).neighboursOfVariableInc(0).toSet === Set(0,1,2)) ^
    p^
    "treedness" ^
      "2x2 grid is no tree" ! (grid(2,2).isTree must beFalse) ^
      "10x1 grid must be tree" ! (grid(10,1).isTree must beTrue) ^
      "foo" ! Prop.forAll(treeProblem) (_.isTree)
  }
}
