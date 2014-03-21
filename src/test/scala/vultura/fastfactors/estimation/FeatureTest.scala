package vultura.fastfactors.estimation

import org.specs2.Specification
import org.specs2.specification.Fragments
import vultura.fastfactors.{FastFactor, NormalD}

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class FeatureTest extends Specification {
  val domain4 = Array(2,2,2,2)
  override def is: Fragments =
    "condition feature" ^
      (Feature(Array(0,1),Array(0,0)).condition(Map(0->0)) === Some(Feature(Array(1), Array(0)))) ^
      (Feature(Array(0,1),Array(0,1)).condition(Map(0->0)) === Some(Feature(Array(1), Array(1)))) ^
      (Feature(Array(0,1),Array(0,1)).condition(Map(1->0)) === None) ^
    p^
    "feature to factor" ^
      (Feature(Array(0,1),Array(0,0)).toFastFactor(Array(2,2),NormalD,2) === FastFactor(Array(0,1),Array(2,1,1,1)))
}
