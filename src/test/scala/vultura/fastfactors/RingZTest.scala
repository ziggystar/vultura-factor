package vultura.fastfactors

import org.specs2._
import org.specs2.specification.Fragments

import scala.reflect.ClassTag

/**
 * Tests that work for all instances of RingZ.
 */
class RingZTest extends Specification {

  override def is: Fragments =
    "NormalD tests" ^
      test(NormalD) ^
    p^
    "LogD tests" ^
      test(LogD)


  def test[T: ClassTag](ring: RingZ[T]): Fragments =
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
      ring.expectation(d,v) === 0
    } ^
    "expectation with inconsistent distribution yields zero" ! {
      val d = Array(ring.zero)
      val v = Array(ring.one)
      ring.logExpectation(d,v) === 0
    }
}
