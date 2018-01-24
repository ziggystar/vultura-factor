package vultura.factor

import org.specs2.mutable._

class NormalDTest extends Specification {
  "log expectation" should {
    "with zeros" ! (NormalD.logExpectation(Array(0d, 1), Array(0d, 1)) === 0d)
    "some test" ! (NormalD.logExpectation(Array(0.2,0.8), Array(1,2)) === 0.2 * math.log(1) + 0.8 * math.log(2))
    "of constant zero should yield -Inf" ! (NormalD.logExpectation(Array(0,0),Array(0,0)) === Double.NegativeInfinity)
    "of one/zero should be -Inf" ! (NormalD.logExpectation(Array(1d),Array(0d)) === Double.NegativeInfinity)
  }
  "expectation" should {
    "test 1" ! (NormalD.expectation(Array(0d,1d),Array(0.3,0.7)) === 0.7)
    "must perform normalization" ! (NormalD.expectation(Array(1,2),Array(3,4)) must beCloseTo(0.3333333 * 3 + 0.66666666 * 4, 1e-3))
  }
}
