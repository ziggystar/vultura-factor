package vultura.factor.inference

import org.specs2.mutable.Specification
import vultura.factor.{Factor, NormalD, Problem}
import vultura.inference.ExactByEnumeration

class ExactByEnumerationTest extends Specification {
  "on single factor problem" >> {
    val p = Problem(IndexedSeq(Factor(Array(0,1), Array(1d,2d,3d,4d))),Array(2,2),NormalD)
    val result = ExactByEnumeration(p)
    "partition function" >> (result.logZ must beCloseTo(math.log(10d),1e-12))
    "marginals" >> (result.encodedVarBelief(0) === Factor(Array(0),Array(0.4,0.6)))
  }

  "two independent variables" >> {
    val p = Problem(
      IndexedSeq(
        Factor(Array(0), Array(1d,2d)),
        Factor(Array(1), Array(2d,3d))),
      Array(2,2),
      NormalD)
    val result = ExactByEnumeration(p)
    "partition function" >> (result.logZ must beCloseTo(math.log(2d + 3d + 4d + 6d),1e-12))
    "marginals" >>
      (result.encodedVarBelief(0) === Factor(Array(0),Array(1d/3d,2d/3d))).and(
        result.encodedVarBelief(1) === Factor(Array(1),Array(2d/5d, 3d/5d)))
  }
}
