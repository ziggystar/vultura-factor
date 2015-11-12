package vultura.factor


import org.specs2.mutable.Specification

import scala.reflect.ClassTag

/**
 * Tests that work for all instances of RingZ.
 */
class RingTest extends Specification {

  "NormalD tests" >> test(NormalD)
  "LogD tests" >> test(LogD)


  def test[T: ClassTag](ring: Ring[T]) = {
    "normalize inconsistent distribution does nothing" ! {
      val d = Array.fill(1)(ring.zero)
      ring.normalize(d).deep === d.deep
    } ^
      "normalize inplace inconsistent distribution does nothing" ! {
        val d = Array.fill(1)(ring.zero)
        val d2 = d.clone()
        ring.normalizeInplace(d2)
        d2.deep === d.deep
      } ^
      "normalize deterministic dist does nothing" ! {
        val d = Array(ring.zero, ring.one)
        ring.normalize(d).deep == d.deep
      } ^
      "entropy must handle zeros properly" ! {
        val d = Array(ring.zero, ring.one)
        ring.entropy(d) === 0
      } ^
      "expectation with inconsistent distribution yields zero" ! {
        val d = Array(ring.zero)
        val v = Array(ring.one)
        ring.expectation(d, v) === 0
      } ^
      "expectation with inconsistent distribution yields zero" ! {
        val d = Array(ring.zero)
        val v = Array(ring.one)
        ring.logExpectation(d, v) === 0
      } ^
      "summing over zeros must yield zero" ! {
        val d = Array.fill(4)(ring.zero)
        ring.sumA(d) === ring.zero
      } ^
      "entropy of inconsistent distribution is zero" ! {
        val d = Array.fill(4)(ring.zero)
        ring.entropy(d) === 0d
      }
  }
}
