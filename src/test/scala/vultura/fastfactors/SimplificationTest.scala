package vultura.fastfactors

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.fastfactors.inference.conditioned.GridProblem

/**
 * Tests checking for correct simplification of problems
 *
 * Created by Felix on 13.03.14.
 */
class SimplificationTest extends Specification {

  val gpProblem6x6 = GridProblem(6, 1, 1, 1d, 0, 4)

  override def is: Fragments =
    "correct number of factors" ! (gpProblem6x6.problem.simplify.factors.size === (gpProblem6x6.problem.factors.size - (gpProblem6x6.width * gpProblem6x6.width))) ^
      "Exact result should not change" ! (gpProblem6x6.problem.logZ must beCloseTo(gpProblem6x6.problem.simplify.logZ, 1e-5))
}
