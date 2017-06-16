package vultura.util

import org.specs2._

class SSetTest extends Specification {
  val sset1 = new SSet(Set(Set(1,2,3),Set(2,3,4),Set(3,4,5),Set(1),Set(2),Set(3),Set(2,3)))
  def is =
    "find all supersets (1)" ! (sset1.superSetsOf(Set(2,3)) === Set(Set(2,3),Set(1,2,3),Set(2,3,4))) ^
    "find all supersets (2)" ! (sset1.superSetsOf(Set(3)) === Set(Set(2,3),Set(1,2,3),Set(2,3,4),Set(3,4,5),Set(3))) ^
    "all maximal sets" ! (sset1.maximalSets === Set(Set(1,2,3),Set(2,3,4),Set(3,4,5)))
}
