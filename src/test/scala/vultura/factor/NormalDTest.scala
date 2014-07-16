package vultura.factor

import org.specs2._
import org.specs2.specification.Fragments

/**
 * Created by thomas on 26.06.14.
 */
class NormalDTest extends Specification {
  override def is: Fragments =
    "log expectation with zeros" ! (NormalD.logExpectation(Array(0d,1), Array(0d,1)) === 0d)
}
