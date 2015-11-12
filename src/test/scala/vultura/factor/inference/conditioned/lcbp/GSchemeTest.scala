package vultura.factor.inference.conditioned.lcbp

import org.specs2.mutable.Specification


/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class GSchemeTest extends Specification {

  val domains = Array.fill(10)(2)
  val threeVars = GScheme(Map(
    1 -> Set(2, 3),
    2 -> Set(2),
    3 -> Set(3)
  ), domains)

    "test 1" >> (threeVars.jointConditions(Seq(2,3)).size === 4)
    "test 2" >> (threeVars.subConditions(Map(2 -> 0),Seq(1)) === Seq(Map(2->0,3->0),Map(2->0,3->1)))
    "test 3" >> (threeVars.superCondition(2,Map(2->0,3->1)) === Map(2->0))
  /*^
    (Seq(1,2,3).map(v => v -> threeVars.conditionalContributions(IndexedSeq(Map(2->0,3->0),Map(2->0,3->1)),Map(),v)) ===
      Seq(1 -> IndexedSeq(Set(Map(2->0,3->0)),Set(Map(2->0,3->1))),2 -> IndexedSeq(Set(),Set()), 3 -> IndexedSeq(Set(Map(3->0)),Set(Map(3->1)))))*/

}
