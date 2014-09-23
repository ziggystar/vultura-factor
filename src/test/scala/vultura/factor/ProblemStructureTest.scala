package vultura.factor

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.factor.generators._

class ProblemStructureTest extends Specification {

  override def is: Fragments = {
    "neighbours on 2x2 grid" ^
      "exclusive" ! (grid(2,2).neighboursOfVariableEx(0).toSet === Set(1,2)) ^
      "inclusive" ! (grid(2,2).neighboursOfVariableInc(0).toSet === Set(0,1,2))
  }
}
