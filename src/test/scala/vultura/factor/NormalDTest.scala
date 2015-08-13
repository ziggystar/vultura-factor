package vultura.factor

import org.specs2._

class NormalDTest extends Specification {
  override def is =
    "log expectation with zeros" ! (NormalD.logExpectation(Array(0d,1), Array(0d,1)) === 0d)
}
