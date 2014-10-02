package vultura.factor.inference.conditioned.lcbp

import org.scalacheck.{Gen, Prop}
import org.specs2.{ScalaCheck, Specification}
import org.specs2.specification.Fragments
import vultura.factor.{FactorMatchers, Problem, SCProblemGen}

/**
 * Compare results accross different implementations of LCBP.
 */
class Comparison extends Specification with ScalaCheck with FactorMatchers with SCProblemGen {
  def randomScheme(problemGen: Gen[Problem]): Gen[FactoredScheme] = Gen.sized(n =>
    for{
      p <- problemGen
      conditioners <- Gen.listOfN(n, Gen.oneOf(p.variables))
      dist <- Gen.choose(0,3)
    } yield FactoredScheme.withMaxDistance(conditioners.toSet, dist, p)
  )

  override def is: Fragments =
    "on tree problems" ^
      "old lcbp vs jt-lcbp" ^
        "small trees" ! (Prop.forAll(randomScheme(treeProblem)){scheme =>
          val old = LCBP(scheme)
          val jt = new LCBPGeneral(scheme, tol=1e-12)
          old.logZ.aka("logz of old implementation") must beCloseTo(jt.logZ, 1e-4)
        }).set(maxSize = 5, minTestsOk = 20) ^
      "small grids" ! Prop.forAll(
        randomScheme(gridProblem)
          .map(scheme => (LCBP(scheme), new LCBPGeneral(scheme, tol = 1e-12)))
          .filter(x => x._1.calibrator.isConverged && x._2.calibrator.isConverged)
      ){ case (old,jt) => old.logZ must beCloseTo(jt.logZ, 1e-6) }
}
