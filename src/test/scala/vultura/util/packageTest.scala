package vultura.util

import org.specs2.Specification
import org.specs2.specification.Fragments

/**
 * @author Thomas Geier <thomas.geier@uni-ulm.de>
 */
class packageTest extends Specification {
  override def is: Fragments =
    "tests for disjoint set of sets" ^
      (Set(Set(1),Set(2)).isPairwiseDisjoint must beTrue) ^
      (Set(Set(1,2),Set(2)).isPairwiseDisjoint must beFalse) ^
      (Set(Set[Int](),Set(2)).isPairwiseDisjoint must beTrue) ^
      (Set(Set(1),Set(2),Set(3)).isPairwiseDisjoint must beTrue) ^
      (Set(Set(1),Set(2),Set(3,2,1)).isPairwiseDisjoint must beFalse) ^
      (Set(Set(1)).isPairwiseDisjoint must beTrue) ^
      (Set(Set[Int]()).isPairwiseDisjoint must beTrue)
}
