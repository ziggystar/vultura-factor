package vultura.fastfactors.algorithms.conditioned

import org.specs2.Specification
import org.specs2.specification.Fragments

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class GSchemeTest extends Specification {
  import LScheme._

  val domains = Array.fill(10)(2)
  val threeVars = GScheme(Map(
    1 -> DCon(split(2,domains),split(3,domains)),
    2 -> split(2,domains),
    3 -> split(3,domains)
  ))

  override def is: Fragments =
    (threeVars.jointConditions(Seq(2,3)).size === 4) ^
    (threeVars.subConditions(Map(2 -> 0),Seq(1)) === Seq(Map(2->0,3->0),Map(2->0,3->1))) ^
    (threeVars.superCondition(2,Map(2->0,3->1)) === Map(2->0)) ^
    (threeVars.conditionalContributions(IndexedSeq(Map(2->0,3->0),Map(2->0,3->1)),Map()) ===
      Seq(1 -> IndexedSeq(Set(Map(2->0,3->0)),Set(Map(2->0,3->1))), 3 -> IndexedSeq(Set(Map(3->0)),Set(Map(3->1)))))

}
